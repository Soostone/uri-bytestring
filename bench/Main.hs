{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

-------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.DeepSeq
import           Criterion.Main
import           Data.String
import qualified Network.URI              as NU
-------------------------------------------------------------------------------
import           URI.ByteString
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
  [
    bgroup "parsing"
      [
        bench "Network.URI.parseURI" $ nf NU.parseURI exampleURIS
      , bench "URI.ByteString.parseURI strict" $ nf (parseURI strictURIParserOptions) exampleURIS
      , bench "URI.ByteString.parseURI lax" $ nf (parseURI laxURIParserOptions) exampleURIS
      , bench "URI.ByteString.parseRelativeRef strict" $ nf (parseRelativeRef strictURIParserOptions) exampleRelativeRefS
      , bench "URI.ByteString.parseRelativeRef lax" $ nf (parseRelativeRef laxURIParserOptions) exampleRelativeRefS
      ]
  , bgroup "serializing"
    [
      bench "URI.ByteString.serializeURIRef on URI" $ nf (toLazyByteString . serializeURIRef) exampleURI
    , bench "URI.ByteString.serializeURIRef on relative ref" $ nf (toLazyByteString . serializeURIRef) exampleRelativeRef
    ]
  ]


exampleURIS :: IsString s => s
exampleURIS = "http://google.com/example?params=youbetcha"


exampleRelativeRefS :: IsString s => s
exampleRelativeRefS = "/example?params=youbetcha#17u"


exampleURI :: URI
exampleURI = URI {
      uriScheme = Scheme "http"
    , uriAuthority = Just Authority {
          authorityUserInfo = Nothing
        , authorityHost = Host "google.com"
        , authorityPort = Nothing
        }
    , uriPath = "/example"
    , uriQuery = Query [("params", "youbetcha")]
    , uriFragment = Nothing
    }


exampleRelativeRef :: RelativeRef
exampleRelativeRef = RelativeRef {
      rrAuthority = Just Authority {
          authorityUserInfo = Nothing
        , authorityHost = Host "google.com"
        , authorityPort = Nothing
        }
    , rrPath = "/example"
    , rrQuery = Query [("params", "youbetcha")]
    , rrFragment = Nothing
    }
