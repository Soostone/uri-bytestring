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
instance NFData Authority
instance NFData Host
instance NFData UserInfo
instance NFData SchemaError
instance NFData URIParseError
instance NFData Scheme
instance NFData Port
instance NFData Query

instance NFData (URIRef a) where
  rnf (URI a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e
  rnf (RelativeRef b c d e) = rnf b `seq` rnf c `seq` rnf d `seq` rnf e


-------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
  [
    bgroup "parsing"
      [
        bench "Network.URI.parseURI" $ nf NU.parseURI exampleURIS
      , bench "URI.ByteString.parseURI strict" $ nf (parseURI strictURIParserOptions) exampleURIS
      , bench "URI.ByteString.parseURI lax" $ nf (parseURI laxURIParserOptions) exampleURIS
      , bench "URI.ByteString.parseURI strict (unparsed query string)" $ nf (parseURI strictURIParserOptions') exampleURIS
      , bench "URI.ByteString.parseURI lax (unparsed query string)" $ nf (parseURI laxURIParserOptions') exampleURIS
      , bench "URI.ByteString.parseRelativeRef strict" $ nf (parseRelativeRef strictURIParserOptions) exampleRelativeRefS
      , bench "URI.ByteString.parseRelativeRef lax" $ nf (parseRelativeRef laxURIParserOptions) exampleRelativeRefS
      ]
  , bgroup "serializing"
    [
      bench "URI.ByteString.serializeURIRef on URI" $ nf (toLazyByteString . serializeURIRef) exampleURI
    , bench "URI.ByteString.serializeURIRef on URI (unparsed query string)" $ nf (toLazyByteString . serializeURIRef) $ exampleURI { uriQuery = QueryString "params=youbetcha" }
    , bench "URI.ByteString.serializeURIRef on relative ref" $ nf (toLazyByteString . serializeURIRef) exampleRelativeRef
    , bench "URI.ByteString.serializeURIRef on relative ref (unparsed query string)" $ nf (toLazyByteString . serializeURIRef) $ exampleRelativeRef { rrQuery = QueryString "params=youbetcha" }
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

