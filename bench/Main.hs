{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

-------------------------------------------------------------------------------
import           Control.DeepSeq.Generics
import           Criterion.Main
import           Data.Attoparsec.ByteString
import           Data.String
import qualified Network.URI                as NU
-------------------------------------------------------------------------------
import           URI.ByteString
import           URI.Parser
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
    bench "Network.URI.parseURI" $ nf NU.parseURI exampleUri
  , bench "URI.ByteString.parseURI strict" $ nf (parseURI strictURIParserOptions) exampleUri
  , bench "URI.Parser.uriParser strict" $ nf (parseOnly $ uriParser strictURIParserOptions) exampleUri
  ]


exampleUri :: IsString s => s
exampleUri = "http://google.com/example?params=youbetcha"
