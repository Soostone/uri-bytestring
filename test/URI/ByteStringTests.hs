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
  , testParseFailure "$$$$://www.example.org/" (MalformedScheme NonAlphaLeading)
                                   ]

uriParseErrorInstancesTests :: TestTree
uriParseErrorInstancesTests = testGroup "URIParseError instances" [
    testProperty "roundtrips between Show and Read" $ \(e :: URIParseError) ->
      read (show e) == e
                                                                  ]

testParses :: ByteString -> URI -> TestTree
testParses s = parseTest s . Right

testParseFailure :: ByteString -> URIParseError -> TestTree
testParseFailure s = parseTest s . Left

parseTest :: ByteString -> Either URIParseError URI -> TestTree
parseTest s r = testCase (unpack s) $ parseURI s @?= r
