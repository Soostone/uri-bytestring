{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Criterion.Main
import URI.ByteString

main :: IO ()
main = defaultMain [
  bench "parse" $ nf parseUri "http://google.com/example?params=youbetcha"
                   ]
