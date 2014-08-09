{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module URI.ByteStringTests (tests) where

import Data.ByteString ( ByteString )
import Data.ByteString.Char8 ( unpack )
import Data.DeriveTH (derive)
import Data.Derive.Arbitrary (makeArbitrary)
import Data.Monoid
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import URI.ByteString

$(derive makeArbitrary ''SchemaError)
$(derive makeArbitrary ''URIParseError)

tests :: TestTree
tests = testGroup "URI.Bytestring" [
    parseUriTests
  , uriParseErrorInstancesTests
                                   ]

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
  , testParses "https://user:pass:wo%20rd@www.example.org?foo=bar&foo=baz+quux#frag" $
      URI (Scheme "https")
          (Just (Authority (Just (UserInfo "user" "pass:wo rd")) (Host "www.example.org") Nothing))
          ""
          (Query [("foo", Just "bar"), ("foo", Just "baz quux")])
          (Just "frag")
                                   ]

uriParseErrorInstancesTests :: TestTree
uriParseErrorInstancesTests = testGroup "URIParseError instances" [
    testProperty "roundtrips between Show and Read" $ \(e :: URIParseError) ->
      read (show e) == e
  , testCase "reads escaped strings into OtherError" $
      read "\"this is why fail is terrible\"" @?= OtherError "this is why fail is terrible"
                                                                  ]

testParses :: ByteString -> URI -> TestTree
testParses s u = testCase (unpack s) $ parseUri s @?= Right u
