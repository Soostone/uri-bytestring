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
$(derive makeArbitrary ''QueryParseError)
$(derive makeArbitrary ''URIParseError)

tests :: TestTree
tests = testGroup "URI.Bytestring" [
    parseURITests
  , parseQueryTests
  , uriParseErrorInstancesTests
                                   ]

parseURITests :: TestTree
parseURITests = testGroup "parseURI" [
    testParsesURI "http://www.example.org/" $
      URI (Scheme "http")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          "/"
          mempty
          Nothing
  , testParsesURI "http://www.example.org" $
      URI (Scheme "http")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          ""
          mempty
          Nothing
  , testParsesURI "https://user:pass:wo%20rd@www.example.org?foo=bar&foo=baz+quux#frag" $
      URI (Scheme "https")
          (Just (Authority (Just (UserInfo "user" "pass:wo rd")) (Host "www.example.org") Nothing))
          ""
          (Query [("foo", Just "bar"), ("foo", Just "baz quux")])
          (Just "frag")
  , testParsesURI "http://www.google.com/aclk?sa=l&ai=CChPOVvnoU8fMDI_QsQeE4oGwDf664-EF7sq01HqV1MMFCAAQAigDUO3VhpcDYMnGqYvApNgPoAGq3vbiA8gBAaoEKE_QQwekDUoMeW9IQghV4HRuzL_l-7vVjlML559kix6XOcC1c4Tb9xeAB76hiR2QBwGoB6a-Gw&sig=AOD64_3Ulyu0DcDsc1AamOIxq63RF9u4zQ&rct=j&q=&ved=0CCUQ0Qw&adurl=http://www.aruba.com/where-to-stay/hotels-and-resorts%3Ftid%3D122"
      URI { uriScheme = Scheme {getScheme = "http"}
          , uriAuthority = Just Authority {authorityUserInfo = Nothing, authorityHost = Host {getHost = "www.google.com"}, authorityPort = Nothing}
          , uriPath = "/aclk"
          , uriQuery = Query {getQuery =
              [("sa",Just "l")
              ,("ai",Just "CChPOVvnoU8fMDI_QsQeE4oGwDf664-EF7sq01HqV1MMFCAAQAigDUO3VhpcDYMnGqYvApNgPoAGq3vbiA8gBAaoEKE_QQwekDUoMeW9IQghV4HRuzL_l-7vVjlML559kix6XOcC1c4Tb9xeAB76hiR2QBwGoB6a-Gw")
              ,("sig",Just "AOD64_3Ulyu0DcDsc1AamOIxq63RF9u4zQ")
              ,("rct",Just "j")
              ,("q",Nothing)
              ,("ved",Just "0CCUQ0Qw")
              ,("adurl",Just "http://www.aruba.com/where-to-stay/hotels-and-resorts?tid=122")
              ]}
          , uriFragment = Nothing
          }

  , testParseFailure "$$$$://www.example.org/" (MalformedScheme NonAlphaLeading)
                                   ]
parseQueryTests :: TestTree
parseQueryTests = testGroup "parseQuery" [
    testParsesQuery "sa=l&ai=CChPOVvnoU8fMDI_QsQeE4oGwDf664-EF7sq01HqV1MMFCAAQAigDUO3VhpcDYMnGqYvApNgPoAGq3vbiA8gBAaoEKE_QQwekDUoMeW9IQghV4HRuzL_l-7vVjlML559kix6XOcC1c4Tb9xeAB76hiR2QBwGoB6a-Gw&sig=AOD64_3Ulyu0DcDsc1AamOIxq63RF9u4zQ&rct=j&q=&ved=0CCUQ0Qw&adurl=http://www.aruba.com/where-to-stay/hotels-and-resorts%3Ftid%3D122" Query {getQuery =
              [("sa",Just "l")
              ,("ai",Just "CChPOVvnoU8fMDI_QsQeE4oGwDf664-EF7sq01HqV1MMFCAAQAigDUO3VhpcDYMnGqYvApNgPoAGq3vbiA8gBAaoEKE_QQwekDUoMeW9IQghV4HRuzL_l-7vVjlML559kix6XOcC1c4Tb9xeAB76hiR2QBwGoB6a-Gw")
              ,("sig",Just "AOD64_3Ulyu0DcDsc1AamOIxq63RF9u4zQ")
              ,("rct",Just "j")
              ,("q",Nothing)
              ,("ved",Just "0CCUQ0Qw")
              ,("adurl",Just "http://www.aruba.com/where-to-stay/hotels-and-resorts?tid=122")
              ]}
  , testParsesQuery "?sa=l&ai=CChPOVvnoU8fMDI_QsQeE4oGwDf664-EF7sq01HqV1MMFCAAQAigDUO3VhpcDYMnGqYvApNgPoAGq3vbiA8gBAaoEKE_QQwekDUoMeW9IQghV4HRuzL_l-7vVjlML559kix6XOcC1c4Tb9xeAB76hiR2QBwGoB6a-Gw&sig=AOD64_3Ulyu0DcDsc1AamOIxq63RF9u4zQ&rct=j&q=&ved=0CCUQ0Qw&adurl=http://www.aruba.com/where-to-stay/hotels-and-resorts%3Ftid%3D122" Query {getQuery =
              [("sa",Just "l")
              ,("ai",Just "CChPOVvnoU8fMDI_QsQeE4oGwDf664-EF7sq01HqV1MMFCAAQAigDUO3VhpcDYMnGqYvApNgPoAGq3vbiA8gBAaoEKE_QQwekDUoMeW9IQghV4HRuzL_l-7vVjlML559kix6XOcC1c4Tb9xeAB76hiR2QBwGoB6a-Gw")
              ,("sig",Just "AOD64_3Ulyu0DcDsc1AamOIxq63RF9u4zQ")
              ,("rct",Just "j")
              ,("q",Nothing)
              ,("ved",Just "0CCUQ0Qw")
              ,("adurl",Just "http://www.aruba.com/where-to-stay/hotels-and-resorts?tid=122")
              ]}
                                         ]

uriParseErrorInstancesTests :: TestTree
uriParseErrorInstancesTests = testGroup "URIParseError instances" [
    testProperty "roundtrips between Show and Read" $ \(e :: URIParseError) ->
      read (show e) == e
                                                                  ]

testParsesURI :: ByteString -> URI -> TestTree
testParsesURI s = parseURITest s . Right

testParsesQuery :: ByteString -> Query -> TestTree
testParsesQuery s = parseQueryTest s . Right

testParseFailure :: ByteString -> URIParseError -> TestTree
testParseFailure s = parseURITest s . Left

parseURITest :: ByteString -> Either URIParseError URI -> TestTree
parseURITest = parseTest parseURI

parseQueryTest :: ByteString -> Either QueryParseError Query -> TestTree
parseQueryTest = parseTest parseQuery

parseTest :: (Eq a, Show a) => (ByteString -> a) -> ByteString -> a -> TestTree
parseTest parser s r = testCase (unpack s) $ parser s @?= r

