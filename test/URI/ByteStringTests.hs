{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module URI.ByteStringTests (tests) where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Derive.Arbitrary (makeArbitrary)
import           Data.DeriveTH         (derive)
import           Data.Monoid
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           URI.ByteString

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
          (Query [("foo", "bar"), ("foo", "baz quux")])
          (Just "frag")
  , testParses "http://www.google.com/aclk?sa=l&ai=CChPOVvnoU8fMDI_QsQeE4oGwDf664-EF7sq01HqV1MMFCAAQAigDUO3VhpcDYMnGqYvApNgPoAGq3vbiA8gBAaoEKE_QQwekDUoMeW9IQghV4HRuzL_l-7vVjlML559kix6XOcC1c4Tb9xeAB76hiR2QBwGoB6a-Gw&sig=AOD64_3Ulyu0DcDsc1AamOIxq63RF9u4zQ&rct=j&q=&ved=0CCUQ0Qw&adurl=http://www.aruba.com/where-to-stay/hotels-and-resorts%3Ftid%3D122"
      URI { uriScheme = Scheme {getScheme = "http"}
          , uriAuthority = Just Authority {authorityUserInfo = Nothing, authorityHost = Host {getHost = "www.google.com"}, authorityPort = Nothing}
          , uriPath = "/aclk"
          , uriQuery = Query {getQuery =
              [("sa", "l")
              ,("ai", "CChPOVvnoU8fMDI_QsQeE4oGwDf664-EF7sq01HqV1MMFCAAQAigDUO3VhpcDYMnGqYvApNgPoAGq3vbiA8gBAaoEKE_QQwekDUoMeW9IQghV4HRuzL_l-7vVjlML559kix6XOcC1c4Tb9xeAB76hiR2QBwGoB6a-Gw")
              ,("sig", "AOD64_3Ulyu0DcDsc1AamOIxq63RF9u4zQ")
              ,("rct", "j")
              ,("q", "")
              ,("ved", "0CCUQ0Qw")
              ,("adurl", "http://www.aruba.com/where-to-stay/hotels-and-resorts?tid=122")
              ]}
          , uriFragment = Nothing
          }

  , testParseFailure "$$$$://www.example.org/" (MalformedScheme NonAlphaLeading)
  , testParses "http://www.example.org/foo#bar" $
      URI (Scheme "http")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          "/foo"
          mempty
          (Just "bar")
  , testParseFailure "http://www.example.org/foo#bar#baz" MalformedFragment
  , testParseFailure "https://www.example.org?listParam[]=foo,bar" MalformedQuery
  , testParsesLax "https://www.example.org?listParam[]=foo,bar" $
          URI (Scheme "https")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          ""
          (Query [("listParam[]", "foo,bar")])
          Nothing

  , testParses "https://www.example.org?listParam%5B%5D=foo,bar" $
      URI (Scheme "https")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          ""
          (Query [("listParam[]", "foo,bar")])
          Nothing
                                   ]

uriParseErrorInstancesTests :: TestTree
uriParseErrorInstancesTests = testGroup "URIParseError instances" [
    testProperty "roundtrips between Show and Read" $ \(e :: URIParseError) ->
      read (show e) == e
                                                                  ]

testParses :: ByteString -> URI -> TestTree
testParses = testParses' strictURIParserOptions

testParsesLax :: ByteString -> URI -> TestTree
testParsesLax = testParses' laxURIParserOptions

testParses' :: URIParserOptions -> ByteString -> URI -> TestTree
testParses' opts s = parseTest opts s . Right

testParseFailure :: ByteString -> URIParseError -> TestTree
testParseFailure s = parseTest strictURIParserOptions s . Left

parseTest :: URIParserOptions -> ByteString -> Either URIParseError URI -> TestTree
parseTest opts s r = testCase (unpack s) $ parseURI opts s @?= r
