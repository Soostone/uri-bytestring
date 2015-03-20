{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

-------------------------------------------------------------------------------
import           Control.DeepSeq.Generics
import           Criterion.Main
import           Data.String
import qualified Network.URI                as NU
-------------------------------------------------------------------------------
import           URI.ByteString
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
instance NFData Authority
instance NFData UserInfo
instance NFData URI
instance NFData NU.URI
instance NFData SchemaError
instance NFData URIParseError


-------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
  [
    bgroup "parsing"
      [
        bench "Network.URI.parseURI" $ nf NU.parseURI exampleURIS
      , bench "URI.ByteString.parseURI strict" $ nf (parseURI strictURIParserOptions) exampleURIS
      , bench "URI.ByteString.parseURI lax" $ nf (parseURI laxURIParserOptions) exampleURIS
      ]
  , bgroup "serializing"
    [
      bench "URI.ByteString.serializeURI" $ nf serializeURI exampleURI
    ]
  ]


exampleURIS :: IsString s => s
exampleURIS = "http://google.com/example?params=youbetcha"


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
