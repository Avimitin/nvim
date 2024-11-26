#!/usr/bin/env nix
#!nix shell .#ghc-for-ts-plugins -c runghc

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Foldl as Fold
import Data.Aeson
import Data.ByteString.Builder
import Data.Maybe (fromJust)
import Data.Text (splitOn, strip)
import Data.Text.Encoding
import GHC.Generics
import Turtle

data DerivationInfo = DerivationInfo
    { name :: Text
    , url :: Text
    , hash :: Text
    }
    deriving (Generic, Show)

instance FromJSON DerivationInfo

getSrcInfo _ = do
    rawJson <-
        inproc
            "nix"
            [ "eval"
            , "--json"
            , ".#treesitter-plugin-nightly"
            , "--apply"
            , "pkg: map (p: { name = p.name; url = p.src.url; hash = p.src.outputHash; }) pkg.paths"
            ]
            empty
    let infos = decode $ toLazyByteString $ encodeUtf8Builder $ lineToText rawJson :: Maybe [DerivationInfo]
     in case infos of
            Just a -> return a
            Nothing -> die "fail parsing JSON value, invalid nix output"

nixPrefetch url = do
    filepath <-
        fold (inproc "nix-prefetch-url" [url, "--print-path", "--type", "sha256"] empty) Fold.last
    case filepath of
        Just path -> return $ lineToText path
        Nothing -> die "nix-prefetch-url doesn't return expected path"

nixHash filepath = do
    hash <-
        fold
            (inproc "nix" ["hash", "file", "--base16", "--type", "sha256", "--sri", filepath] empty)
            Fold.last
    return $ strip $ lineToText $ fromJust hash

updateHash name old new = do
    printf (s % " hash changed from " % s % " to " % s) name old new
    inplace (text old *> return new) "overlay.nix"

bump _ = do
    echo "Fetching source info"
    allInfos <- getSrcInfo ()
    DerivationInfo{name = pname, url = purl, hash = oldHash} <- select allInfos
    liftIO (print $ format ("Fetching " % s % " from url: " % s) pname purl)
    filepath <- nixPrefetch purl
    newHash <- nixHash filepath
    if oldHash /= newHash
        then sh (updateHash pname oldHash newHash)
        else empty

main = sh $ bump ()
