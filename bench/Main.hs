{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-------------------------------------------------------------------------------
import           Criterion.Main
import           Control.DeepSeq.Generics
-------------------------------------------------------------------------------
import           URI.ByteString
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
instance NFData Authority
instance NFData UserInfo
instance NFData URI
instance NFData SchemaError
instance NFData URIParseError


-------------------------------------------------------------------------------
main :: IO ()
main = defaultMain [
  bench "parse" $ nf parseURI "http://google.com/example?params=youbetcha"
                   ]
