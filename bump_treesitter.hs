#!/usr/bin/env nix
#!nix shell .#ghc-for-ts-plugins -c runghc

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import qualified Control.Concurrent.Async
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import qualified Control.Foldl as Fold
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder
import Data.Functor (($>))
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.IO as TIO
import qualified GHC.Generics
import qualified System.IO
import Turtle

{- | This wrapped in Maybe because I am using Channel based IO for read and write file.
     A `Nothing` constructor could help us exit the dangling updater when finish.
-}
type NewHashInfo = Maybe (Text, Text, Text)

data Env = Env
    { envChan :: Chan NewHashInfo
    , envLock :: MVar ()
    }

type AppM = ReaderT Env IO

data DerivationInfo = DerivationInfo
    { name :: Text
    , url :: Text
    , hash :: Text
    }
    deriving (GHC.Generics.Generic, Show)

instance Aeson.FromJSON DerivationInfo

safePrint :: Text -> AppM ()
safePrint msg = do
    lock <- asks envLock
    liftIO $ withMVar lock $ \_ -> TIO.putStrLn msg

procGetStdout :: Text -> [Text] -> IO Text
procGetStdout cmd args = do
    (exitCode, rawOut, rawErr) <- procStrictWithErr cmd args empty
    case exitCode of
        ExitSuccess -> return $ Data.Text.strip rawOut
        ExitFailure code ->
            die $ "command " <> cmd <> " fail with exit code " <> repr code <> ", stderr: " <> rawErr

getSrcInfo :: a -> IO [DerivationInfo]
getSrcInfo _ = do
    rawJson <-
        procGetStdout
            "nix"
            [ "eval"
            , "--json"
            , ".#treesitter-plugin-nightly"
            , "--apply"
            , "pkg: map (p: { name = p.name; url = p.src.url; hash = p.src.outputHash; }) pkg.plugins"
            ]
    case Aeson.decode $
        Data.ByteString.Builder.toLazyByteString $
            Data.Text.Encoding.encodeUtf8Builder rawJson of
        Just a -> return a
        Nothing -> die "fail parsing JSON value, invalid nix output"

nixPrefetch :: Text -> AppM Text
nixPrefetch url = do
    safePrint $ format ("Exec nix-prefetch-url with url: " % s) url
    liftIO $ do
        rawOut <- procGetStdout "nix-prefetch-url" [url, "--print-path", "--type", "sha256"]
        return $ last $ Data.Text.lines rawOut

nixHash :: Text -> AppM Text
nixHash filepath = do
    safePrint $ format ("Exec nix hash with file: " % s) filepath
    liftIO $ procGetStdout "nix" ["hash", "file", "--base16", "--type", "sha256", "--sri", filepath]

updateHash :: Text -> Text -> Text -> AppM ()
updateHash name old new = do
    chan <- asks envChan
    safePrint $ format (s % " hash changed from " % s % " to " % s) name old new
    liftIO $ writeChan chan $ Just (name, old, new)

updateHashFromChan :: AppM ()
updateHashFromChan = do
    chan <- asks envChan
    msg <- liftIO $ readChan chan
    case msg of
        Just (name, old, new) -> do
            safePrint $ format ("Replacing hash for derivation " % s) name
            liftIO $ inplace (text old $> new) "overlay.nix"
            updateHashFromChan
        Nothing -> do
            safePrint "Bye!"
            return ()

tryUpdateHash :: DerivationInfo -> AppM ()
tryUpdateHash DerivationInfo{name = pname, url = purl, hash = oldHash} = do
    filepath <- nixPrefetch purl
    newHash <- nixHash filepath
    safePrint $ format ("Examinate hash for " % s) pname
    when (oldHash /= newHash) $ do
        updateHash pname oldHash newHash

-- | return a list of async handle for task update
updateOverlayWithAsync ::
    [DerivationInfo] -> ReaderT Env Shell (Control.Concurrent.Async.Async ())
updateOverlayWithAsync originDrvsInfo = do
    drv <- lift $ select originDrvsInfo
    bumpEnv <- ask
    liftIO $ Control.Concurrent.Async.async $ runReaderT (tryUpdateHash drv) bumpEnv

updateOverlay :: [DerivationInfo] -> Shell ()
updateOverlay originDrvsInfo = do
    chan <- liftIO (newChan :: IO (Chan NewHashInfo))
    lock <- liftIO (newMVar () :: IO (MVar ()))
    let bumpEnv = Env chan lock
    _ <- liftIO $ Control.Concurrent.Async.async $ runReaderT updateHashFromChan bumpEnv
    asyncHandles <- flip fold Fold.list $ runReaderT (updateOverlayWithAsync originDrvsInfo) bumpEnv
    liftIO $ mapM_ wait asyncHandles
    liftIO $ writeChan chan Nothing

bump :: IO ()
bump = do
    printf "Start bumping\n"
    allInfos <- getSrcInfo ()
    sh $ updateOverlay allInfos

main :: IO ()
main = do
    System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
    bump

