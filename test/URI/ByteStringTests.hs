{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module URI.ByteStringTests (tests) where

-------------------------------------------------------------------------------
import           Control.Lens
import qualified Blaze.ByteString.Builder as BB
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as B8
import           Data.Monoid
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Prelude
-------------------------------------------------------------------------------
import           URI.ByteString
import           URI.ByteString.Arbitrary ()
-------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "URI.Bytestring"
  [
    parseUriTests
  , uriParseErrorInstancesTests
  , lensTests
  , serializeURITests
  ]


-------------------------------------------------------------------------------
parseUriTests :: TestTree
parseUriTests = testGroup "parseUri"
  [
    testParses "http://www.example.org/" $
      URI (Scheme "http")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          "/"
          mempty
          Nothing
  , testParseHost "http://www.example.org" "www.example.org"
  -- IPV4
  , testParseHost "http://192.168.1.1" "192.168.1.1"
  -- IPV6
  , testParseHost "http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]" "FEDC:BA98:7654:3210:FEDC:BA98:7654:3210"
  , testParseHost "http://[1080:0:0:0:8:800:200C:417A]" "1080:0:0:0:8:800:200C:417A"
  , testParseHost "http://[3ffe:2a00:100:7031::1]" "3ffe:2a00:100:7031::1"
  , testParseHost "http://[::192.9.5.5]" "::192.9.5.5"
  , testParseHost "http://[::FFFF:129.144.52.38]" "::FFFF:129.144.52.38"
  , testParseHost "http://[2010:836B:4179::836B:4179]" "2010:836B:4179::836B:4179"
  , testParseHost "http://[2010:836B:4179::836B:4179]" "2010:836B:4179::836B:4179"
  -- IPVFuture
  , testParseHost "http://[v1.fe80::a+en1]" "v1.fe80::a+en1"
  , testParses "https://user:pass:wo%20rd@www.example.org?foo=bar&foo=baz+quux#frag" $
      URI (Scheme "https")
          (Just (Authority (Just (UserInfo "user" "pass:wo rd")) (Host "www.example.org") Nothing))
          ""
          (Query [("foo", "bar"), ("foo", "baz quux")])
          (Just "frag")
  -- trailing &
  , testParses "http://www.example.org?foo=bar&" $
      URI (Scheme "http")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          ""
          (Query [("foo", "bar")])
          Nothing
  , testParses "http://www.google.com:80/aclk?sa=l&ai=CChPOVvnoU8fMDI_QsQeE4oGwDf664-EF7sq01HqV1MMFCAAQAigDUO3VhpcDYMnGqYvApNgPoAGq3vbiA8gBAaoEKE_QQwekDUoMeW9IQghV4HRuzL_l-7vVjlML559kix6XOcC1c4Tb9xeAB76hiR2QBwGoB6a-Gw&sig=AOD64_3Ulyu0DcDsc1AamOIxq63RF9u4zQ&rct=j&q=&ved=0CCUQ0Qw&adurl=http://www.aruba.com/where-to-stay/hotels-and-resorts%3Ftid%3D122"
      URI { uriScheme = Scheme {schemeBS = "http"}
          , uriAuthority = Just Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "www.google.com"}, authorityPort = Just (Port 80)}
          , uriPath = "/aclk"
          , uriQuery = Query {queryPairs =
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

  , testParses "https://www.example.org#only-fragment" $
      URI (Scheme "https")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          ""
          (Query [])
          (Just "only-fragment")
  ,  testParses "https://www.example.org/weird%20path" $
       URI (Scheme "https")
           (Just (Authority Nothing (Host "www.example.org") Nothing))
           "/weird path"
           (Query [])
           Nothing

  , parseTestURI strictURIParserOptions "http://www.example.org/." $
      Right $ URI
          (Scheme "http")
          (Just (Authority Nothing (Host "www.example.org") Nothing))
          "/."
          (Query [])
          Nothing
  , parseTestURI strictURIParserOptions "http:/." $
      Right $ URI
          (Scheme "http")
          Nothing
          "/."
          (Query [])
          Nothing

  , roundtripTestURI strictURIParserOptions "ftp://ftp.is.co.za/rfc/rfc1808.txt"
  , roundtripTestURI strictURIParserOptions "http://www.ietf.org/rfc/rfc2396.txt"
  , roundtripTestURI strictURIParserOptions "mailto:John.Doe@example.com"
  , roundtripTestURI strictURIParserOptions "news:comp.infosystems.www.servers.unix"
  , roundtripTestURI strictURIParserOptions "tel:+1-816-555-1212"
  , roundtripTestURI strictURIParserOptions "telnet://192.0.2.16:80/"

  -- RFC 3986, Section 4.2
  , parseTestRelativeRef strictURIParserOptions "verysimple" $
      Right $ RelativeRef
          Nothing
          "verysimple"
          (Query [])
          Nothing
  , parseTestRelativeRef strictURIParserOptions "this:that/thap/sub?1=2" $
      Left $ MalformedPath
  , parseTestRelativeRef strictURIParserOptions "./this:that/thap/sub?1=2" $
      Right $ RelativeRef
          Nothing
          "./this:that/thap/sub"
          (Query [("1", "2")])
          Nothing
  ]


-------------------------------------------------------------------------------
uriParseErrorInstancesTests :: TestTree
uriParseErrorInstancesTests = testGroup "URIParseError instances"
  [
    testProperty "roundtrips between Show and Read" $ \(e :: URIParseError) ->
      read (show e) == e
  ]


-------------------------------------------------------------------------------
lensTests :: TestTree
lensTests = testGroup "lenses"
  [
    testProperty "schemeBSL Lens" $ \bs bs' ->
      let wrapped = Scheme bs
      in (wrapped ^. schemeBSL) === schemeBS wrapped .&&.
         (wrapped & schemeBSL .~ bs') === wrapped { schemeBS = bs'}
  , testProperty "hostBSL Lens" $ \bs bs' ->
      let wrapped = Host bs
      in (wrapped ^. hostBSL) === hostBS wrapped .&&.
         (wrapped & hostBSL .~ bs') === wrapped { hostBS = bs'}
  , testProperty "portNumberL Lens" $ \n n' ->
      let wrapped = Port n
      in (wrapped ^. portNumberL) === portNumber wrapped .&&.
         (wrapped & portNumberL .~ n') === wrapped { portNumber = n'}
  , testProperty "queryPairsL Lens" $ \ps ps' ->
      let wrapped = Query ps
      in wrapped ^. queryPairsL === queryPairs wrapped .&&.
         (wrapped & queryPairsL .~ ps') === wrapped { queryPairs = ps'}

  , testProperty "authorityUserInfoL Lens" $ \a ui ->
     (a ^. authorityUserInfoL === authorityUserInfo a) .&&.
     ((a & authorityUserInfoL .~ ui) === a { authorityUserInfo = ui })
  , testProperty "authorityHostL Lens" $ \a host ->
     (a ^. authorityHostL === authorityHost a) .&&.
     ((a & authorityHostL .~ host) === a { authorityHost = host })
  , testProperty "authorityPortL Lens" $ \a port ->
     (a ^. authorityPortL === authorityPort a) .&&.
     ((a & authorityPortL .~ port) === a { authorityPort = port })

  , testProperty "uiUsernameL Lens" $ \ui bs ->
     (ui ^. uiUsernameL === uiUsername ui) .&&.
     ((ui & uiUsernameL .~ bs) === ui { uiUsername = bs })
  , testProperty "uiPasswordL Lens" $ \ui bs ->
     (ui ^. uiPasswordL === uiPassword ui) .&&.
     ((ui & uiPasswordL .~ bs) === ui { uiPassword = bs })

  , testProperty "uriSchemeL Lens" $ \uri x ->
     (uri ^. uriSchemeL === uriScheme uri) .&&.
     ((uri & uriSchemeL .~ x) === uri { uriScheme = x })
  , testProperty "uriAuthorityL Lens" $ \uri x ->
     (uri ^. uriAuthorityL === uriAuthority uri) .&&.
     ((uri & uriAuthorityL .~ x) === uri { uriAuthority = x })
  , testProperty "uriPathL Lens" $ \uri x ->
     (uri ^. uriPathL === uriPath uri) .&&.
     ((uri & uriPathL .~ x) === uri { uriPath = x })
  , testProperty "uriQueryL Lens" $ \uri x ->
     (uri ^. uriQueryL === uriQuery uri) .&&.
     ((uri & uriQueryL .~ x) === uri { uriQuery = x })
  , testProperty "uriFragmentL Lens" $ \uri x ->
     (uri ^. uriFragmentL === uriFragment uri) .&&.
     ((uri & uriFragmentL .~ x) === uri { uriFragment = x })

  , testProperty "rrAuthorityL Lens" $ \rr x ->
     (rr ^. rrAuthorityL === rrAuthority rr) .&&.
     ((rr & rrAuthorityL .~ x) === rr { rrAuthority = x })
  , testProperty "rrPathL Lens" $ \rr x ->
     (rr ^. rrPathL === rrPath rr) .&&.
     ((rr & rrPathL .~ x) === rr { rrPath = x })
  , testProperty "rrQueryL Lens" $ \rr x ->
     (rr ^. rrQueryL === rrQuery rr) .&&.
     ((rr & rrQueryL .~ x) === rr { rrQuery = x })
  , testProperty "rrFragmentL Lens" $ \rr x ->
     (rr ^. rrFragmentL === rrFragment rr) .&&.
     ((rr & rrFragmentL .~ x) === rr { rrFragment = x })
  ]


-------------------------------------------------------------------------------
testParses :: ByteString -> URI -> TestTree
testParses = testParses' strictURIParserOptions


-------------------------------------------------------------------------------
testParseHost :: ByteString -> ByteString -> TestTree
testParseHost uri expectedHost =
  testParses uri $
    URI (Scheme "http")
        (Just (Authority Nothing (Host expectedHost) Nothing))
        mempty
        mempty
        Nothing



-------------------------------------------------------------------------------
testParsesLax :: ByteString -> URI -> TestTree
testParsesLax = testParses' laxURIParserOptions


-------------------------------------------------------------------------------
testParses' :: URIParserOptions -> ByteString -> URI -> TestTree
testParses' opts s u = testGroup "testParses'"
    [ parseTestURI opts s $ Right u
    , parseTestRelativeRef opts (makeRelativeRefBS s) $ Right (makeRelativeRefTyped u)
    ]


-------------------------------------------------------------------------------
makeRelativeRefTyped :: URI -> RelativeRef
makeRelativeRefTyped (URI _ a p q f) = RelativeRef a p q f


-------------------------------------------------------------------------------
makeRelativeRefBS :: ByteString -> ByteString
makeRelativeRefBS s = B8.tail x
  where
    (_, x) = B8.break (==':') s


-------------------------------------------------------------------------------
testParseFailure :: ByteString -> URIParseError -> TestTree
testParseFailure s = parseTestURI strictURIParserOptions s . Left


-------------------------------------------------------------------------------
parseTestURI
    :: URIParserOptions
    -> ByteString
    -> Either URIParseError URI
    -> TestTree
parseTestURI opts s r = testCase (B8.unpack s) $ parseURI opts s @?= r


-------------------------------------------------------------------------------
roundtripTestURI
    :: URIParserOptions
    -> ByteString
    -> TestTree
roundtripTestURI opts s =
    testCase (B8.unpack s) $ (parseURI opts s >>= return . BB.toByteString . serializeURI) @?= Right s


-------------------------------------------------------------------------------
parseTestRelativeRef
    :: URIParserOptions
    -> ByteString
    -> Either URIParseError RelativeRef
    -> TestTree
parseTestRelativeRef opts s r =
  testCase (B8.unpack s) $ parseRelativeRef opts s @?= r


-------------------------------------------------------------------------------
serializeURITests :: TestTree
serializeURITests = testGroup "serializeURI"
  [
    testCase "renders userinfo correctly" $ do
       let ui = UserInfo "user" "pass"
       let uri = URI (Scheme "http")
                 (Just (Authority (Just ui) (Host "www.example.org") Nothing))
                 "/"
                 (Query [("foo", "bar")])
                 (Just "somefragment")
       let res = BB.toLazyByteString (serializeURI uri)
       res @?= "http://user:pass@www.example.org/?foo=bar#somefragment"
  , testCase "encodes decoded paths" $ do
       let uri = URI (Scheme "http")
                 (Just (Authority Nothing (Host "www.example.org") Nothing))
                 "/weird path"
                 (Query [])
                 Nothing
       let res = BB.toLazyByteString (serializeURI uri)
       res @?= "http://www.example.org/weird%20path"
  ]
