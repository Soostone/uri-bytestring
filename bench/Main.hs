{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

-------------------------------------------------------------------------------
import           Control.DeepSeq.Generics
import           Criterion.Main
import           Blaze.ByteString.Builder
import           Data.String
import qualified Network.URI              as NU
-------------------------------------------------------------------------------
import           URI.ByteString
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
instance NFData Authority
instance NFData Host
instance NFData UserInfo
instance NFData URI
instance NFData RelativeRef
instance NFData SchemaError
instance NFData URIParseError
instance NFData Scheme
instance NFData Port
instance NFData Query


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
      bench "URI.ByteString.serializeURI" $ nf (toLazyByteString . serializeURI) exampleURI
    , bench "URI.ByteString.serializeRelativeRef" $ nf (toLazyByteString . serializeRelativeRef) exampleRelativeRef
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
