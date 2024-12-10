#!/usr/bin/env nix
#!nix shell .#ghc-for-ts-plugins -c runghc

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent.Async
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import qualified Control.Foldl as Fold
import qualified Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder
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

data DerivationInfo = DerivationInfo
    { name :: Text
    , url :: Text
    , hash :: Text
    }
    deriving (GHC.Generics.Generic, Show)

instance Aeson.FromJSON DerivationInfo

procGetStdout :: Text -> [Text] -> IO Text
procGetStdout cmd args = do
    (exitCode, stdout, stderr) <- procStrictWithErr cmd args empty
    case exitCode of
        ExitSuccess -> return $ Data.Text.strip stdout
        ExitFailure code ->
            die $ "command " <> cmd <> " fail with exit code " <> repr code <> ", stderr: " <> stderr

getSrcInfo :: a -> IO [DerivationInfo]
getSrcInfo _ = do
    rawJson <-
        procGetStdout
            "nix"
            [ "eval"
            , "--json"
            , ".#treesitter-plugin-nightly"
            , "--apply"
            , "pkg: map (p: { name = p.name; url = p.src.url; hash = p.src.outputHash; }) pkg.paths"
            ]
    case Aeson.decode $
        Data.ByteString.Builder.toLazyByteString $
            Data.Text.Encoding.encodeUtf8Builder rawJson of
        Just a -> return a
        Nothing -> die "fail parsing JSON value, invalid nix output"

nixPrefetch :: Text -> IO Text
nixPrefetch url = do
    TIO.putStrLn $ format ("Exec nix-prefetch-url with url: " % s) url
    stdout <- procGetStdout "nix-prefetch-url" [url, "--print-path", "--type", "sha256"]
    return $ last $ Data.Text.lines stdout

nixHash :: Text -> IO Text
nixHash filepath = do
    TIO.putStrLn $ format ("Exec nix hash with file: " % s) filepath
    procGetStdout "nix" ["hash", "file", "--base16", "--type", "sha256", "--sri", filepath]

updateHash :: Chan NewHashInfo -> Text -> Text -> Text -> IO ()
updateHash chan name old new = do
    TIO.putStrLn $ format (s % " hash changed from " % s % " to " % s) name old new
    liftIO $ writeChan chan $ Just (name, old, new)

updateHashFromChan :: Chan NewHashInfo -> IO ()
updateHashFromChan chan = do
    msg <- readChan chan
    case msg of
        Just (name, old, new) -> do
            inplace (text old *> return new) "overlay.nix"
            updateHashFromChan chan
        Nothing -> do
            TIO.putStrLn "Bye!"
            return ()

tryUpdateHash :: Chan NewHashInfo -> DerivationInfo -> IO ()
tryUpdateHash chan DerivationInfo{name = pname, url = purl, hash = oldHash} = do
    filepath <- nixPrefetch purl
    newHash <- nixHash filepath
    TIO.putStrLn $ format ("Examinate hash for " % s) pname
    when (oldHash /= newHash) $ do
        updateHash chan pname oldHash newHash

-- | return a list of async handle for task update
updateOverlayWithAsync ::
    Chan NewHashInfo -> [DerivationInfo] -> Shell (Control.Concurrent.Async.Async ())
updateOverlayWithAsync chan originDrvsInfo = do
    drv <- select originDrvsInfo
    liftIO $ Control.Concurrent.Async.async $ tryUpdateHash chan drv

updateOverlay :: [DerivationInfo] -> Shell ()
updateOverlay originDrvsInfo = do
    chan <-
        liftIO
            ( do
                chan <- newChan :: IO (Chan NewHashInfo)
                Control.Concurrent.Async.async $ updateHashFromChan chan
                return chan
            )
    asyncHandles <- flip fold Fold.list $ updateOverlayWithAsync chan originDrvsInfo
    mapM_ wait asyncHandles
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
