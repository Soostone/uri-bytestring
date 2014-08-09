{-# LANGUAGE OverloadedStrings #-}
module URI.ByteStringTests (tests) where

import Data.ByteString ( ByteString )
import Data.ByteString.Char8 ( unpack )
import Data.Monoid
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString

tests :: TestTree
tests = testGroup "URI.Bytestring" [parseUriTests]

parseUriTests :: TestTree
parseUriTests = testGroup "parseUri" [
    testParses "http://www.example.org/" $
      URI (Scheme "http")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          "/"
          mempty
          Nothing
  , testParses "http://www.example.org" $
      URI (Scheme "http")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          ""
          mempty
          Nothing
  , testParses "http://www.example.org?foo=bar&foo=baz+quux#frag" $
      URI (Scheme "http")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          ""
          (Query [("foo", Just "bar"), ("foo", Just "baz quux")])
          (Just "frag")
                                   ]

testParses :: ByteString -> URI -> TestTree
testParses s u = testCase (unpack s) $ parseUri s @?= Right u
