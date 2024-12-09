#!/usr/bin/env nix
#!nix shell .#ghc-for-ts-plugins -c runghc

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Foldl as Fold
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.Text as TextLib
import qualified Data.Text.Encoding as TextEncoding
import qualified GHC.Generics as GHCG
import Turtle

data DerivationInfo = DerivationInfo
    { name :: Text
    , url :: Text
    , hash :: Text
    }
    deriving (GHCG.Generic, Show)

instance Aeson.FromJSON DerivationInfo

procGetLastLine :: (MonadIO m) => Text -> [Text] -> m Text
procGetLastLine cmd args = do
    output <- fold (inproc cmd args empty) Fold.last
    case output of
        Just lastLine -> return $ TextLib.strip $ lineToText lastLine
        Nothing -> die ("command " <> cmd <> " doesn't print anything to stdout")

getSrcInfo :: (MonadIO m) => a -> m [DerivationInfo]
getSrcInfo _ = do
    rawJson <-
        procGetLastLine
            "nix"
            [ "eval"
            , "--json"
            , ".#treesitter-plugin-nightly"
            , "--apply"
            , "pkg: map (p: { name = p.name; url = p.src.url; hash = p.src.outputHash; }) pkg.paths"
            ]
    case Aeson.decode $ ByteStringBuilder.toLazyByteString $ TextEncoding.encodeUtf8Builder rawJson of
        Just a -> return a
        Nothing -> die "fail parsing JSON value, invalid nix output"

nixPrefetch :: (MonadIO m) => Text -> m Text
nixPrefetch url = procGetLastLine "nix-prefetch-url" [url, "--print-path", "--type", "sha256"]

nixHash :: (MonadIO m) => Text -> m Text
nixHash filepath = procGetLastLine "nix" ["hash", "file", "--base16", "--type", "sha256", "--sri", filepath]

updateHash :: (MonadIO m) => Text -> Text -> Text -> m ()
updateHash name old new = do
    printf (s % " hash changed from " % s % " to " % s % "\n") name old new
    inplace (text old *> return new) "overlay.nix"

updateOverlay :: [DerivationInfo] -> Shell ()
updateOverlay originDrvsInfo = do
    DerivationInfo{name = pname, url = purl, hash = oldHash} <- select originDrvsInfo
    printf ("Fetching " % s % " from url: " % s % "\n") pname purl
    filepath <- nixPrefetch purl
    newHash <- nixHash filepath
    if oldHash /= newHash
        then do updateHash pname oldHash newHash
        else empty

bump :: (MonadIO io) => io ()
bump = do
    printf "Start bumping\n"
    allInfos <- getSrcInfo ()
    sh $ updateOverlay allInfos

main :: IO ()
main = bump
