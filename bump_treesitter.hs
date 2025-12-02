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
import qualified Turtle

{- | This wrapped in Maybe because I am using Channel based IO for read and write file.
     A `Nothing` constructor could help us exit the dangling updater when finish.
-}
type NewHashInfo = Maybe (Turtle.Text, Turtle.Text, Turtle.Text)

data Env = Env
    { envChan :: Chan NewHashInfo
    , envLock :: MVar ()
    }

type AppM = ReaderT Env IO

data DerivationInfo = DerivationInfo
    { name :: Turtle.Text
    , url :: Turtle.Text
    , hash :: Turtle.Text
    }
    deriving (GHC.Generics.Generic, Show)

instance Aeson.FromJSON DerivationInfo

safePrint :: Turtle.Text -> AppM ()
safePrint msg = do
    lock <- asks envLock
    Turtle.liftIO $ withMVar lock $ \_ -> TIO.putStrLn msg

procGetStdout :: Turtle.Text -> [Turtle.Text] -> IO Turtle.Text
procGetStdout cmd args = do
    (exitCode, rawOut, rawErr) <- Turtle.procStrictWithErr cmd args Turtle.empty
    case exitCode of
        Turtle.ExitSuccess -> return $ Data.Text.strip rawOut
        Turtle.ExitFailure code ->
            Turtle.die $
                "command " <> cmd <> " fail with exit code " <> Turtle.repr code <> ", stderr: " <> rawErr

getSrcInfo :: IO [DerivationInfo]
getSrcInfo = do
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
        Nothing -> Turtle.die "fail parsing JSON value, invalid nix output"

nixPrefetch :: Turtle.Text -> AppM Turtle.Text
nixPrefetch url = do
    safePrint $ Turtle.format ("Exec nix-prefetch-url with url: " Turtle.% Turtle.s) url
    Turtle.liftIO $ do
        rawOut <- procGetStdout "nix-prefetch-url" [url, "--print-path", "--type", "sha256"]
        return $ last $ Data.Text.lines rawOut

nixHash :: Turtle.Text -> AppM Turtle.Text
nixHash filepath = do
    safePrint $ Turtle.format ("Exec nix hash with file: " Turtle.% Turtle.s) filepath
    Turtle.liftIO $
        procGetStdout "nix" ["hash", "file", "--base16", "--type", "sha256", "--sri", filepath]

updateHash :: Turtle.Text -> Turtle.Text -> Turtle.Text -> AppM ()
updateHash name old new = do
    chan <- asks envChan
    safePrint $
        Turtle.format
            (Turtle.s Turtle.% " hash changed from " Turtle.% Turtle.s Turtle.% " to " Turtle.% Turtle.s)
            name
            old
            new
    Turtle.liftIO $ writeChan chan $ Just (name, old, new)

updateHashFromChan :: AppM ()
updateHashFromChan = do
    chan <- asks envChan
    msg <- Turtle.liftIO $ readChan chan
    case msg of
        Just (name, old, new) -> do
            safePrint $ Turtle.format ("Replacing hash for derivation " Turtle.% Turtle.s) name
            Turtle.liftIO $ Turtle.inplace (Turtle.text old $> new) "overlay.nix"
            updateHashFromChan
        Nothing -> do
            safePrint "Bye!"
            return ()

tryUpdateHash :: DerivationInfo -> AppM ()
tryUpdateHash DerivationInfo{name = pname, url = purl, hash = oldHash} = do
    filepath <- nixPrefetch purl
    newHash <- nixHash filepath
    safePrint $ Turtle.format ("Examinate hash for " Turtle.% Turtle.s) pname
    Turtle.when (oldHash /= newHash) $ do
        updateHash pname oldHash newHash

-- | return a list of async handle for task update
updateOverlayWithAsync ::
    [DerivationInfo] -> ReaderT Env Turtle.Shell (Control.Concurrent.Async.Async ())
updateOverlayWithAsync originDrvsInfo = do
    drv <- lift $ Turtle.select originDrvsInfo
    bumpEnv <- ask
    Turtle.liftIO $ Control.Concurrent.Async.async $ runReaderT (tryUpdateHash drv) bumpEnv

updateOverlay :: [DerivationInfo] -> Turtle.Shell ()
updateOverlay originDrvsInfo = do
    chan <- Turtle.liftIO (newChan :: IO (Chan NewHashInfo))
    lock <- Turtle.liftIO (newMVar () :: IO (MVar ()))
    let bumpEnv = Env chan lock
    fileUpdaters <-
        Turtle.liftIO $ Control.Concurrent.Async.async $ runReaderT updateHashFromChan bumpEnv
    asyncHandles <-
        flip Turtle.fold Fold.list $ runReaderT (updateOverlayWithAsync originDrvsInfo) bumpEnv
    Turtle.liftIO $ mapM_ Turtle.wait asyncHandles

    -- Wait for everything to stop
    Turtle.liftIO $ writeChan chan Nothing
    Turtle.liftIO $ Turtle.wait fileUpdaters

bump :: IO ()
bump = do
    Turtle.printf "Start bumping\n"
    allInfos <- getSrcInfo
    Turtle.sh $ updateOverlay allInfos

main :: IO ()
main = do
    System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
    bump
