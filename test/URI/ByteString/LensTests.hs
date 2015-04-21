module URI.ByteString.LensTests
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Lens
import           Data.ByteString.Builder   (toLazyByteString)
import           Test.QuickCheck.Instances
import           Test.Tasty
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           URI.ByteString
import           URI.ByteString.Arbitrary
import           URI.ByteString.Lens
-------------------------------------------------------------------------------



tests :: TestTree
tests = testGroup "URI.ByteString.Lens"
  [
    testProperty "schemeBS Iso" $ \bs ->
      let wrapped = Scheme bs
      in wrapped ^. schemeBS === getScheme wrapped .&&.
         bs ^. from schemeBS === wrapped
  , testProperty "hostBS Iso" $ \bs ->
      let wrapped = Host bs
      in wrapped ^. hostBS === getHost wrapped .&&.
         bs ^. from hostBS === wrapped

  , testProperty "portNumber Iso" $ \bs ->
      let wrapped = Port bs
      in wrapped ^. portNumber === getPort wrapped .&&.
         bs ^. from portNumber === wrapped

  , testProperty "queryPairs Iso" $ \ps ->
      let wrapped = Query ps
      in wrapped ^. queryPairs === getQuery wrapped .&&.
         ps ^. from queryPairs === wrapped

  , testProperty "authorityUserInfo_ Lens" $ \a ui ->
     (a ^. authorityUserInfo_ === authorityUserInfo a) .&&.
     ((a & authorityUserInfo_ .~ ui) === a { authorityUserInfo = ui })
  , testProperty "authorityHost_ Lens" $ \a host ->
     (a ^. authorityHost_ === authorityHost a) .&&.
     ((a & authorityHost_ .~ host) === a { authorityHost = host })
  , testProperty "authorityPort_ Lens" $ \a port ->
     (a ^. authorityPort_ === authorityPort a) .&&.
     ((a & authorityPort_ .~ port) === a { authorityPort = port })

  , testProperty "uiUsername_ Lens" $ \ui bs ->
     (ui ^. uiUsername_ === uiUsername ui) .&&.
     ((ui & uiUsername_ .~ bs) === ui { uiUsername = bs })
  , testProperty "uiPassword_ Lens" $ \ui bs ->
     (ui ^. uiPassword_ === uiPassword ui) .&&.
     ((ui & uiPassword_ .~ bs) === ui { uiPassword = bs })

  , testProperty "uriScheme_ Lens" $ \uri x ->
     (uri ^. uriScheme_ === uriScheme uri) .&&.
     ((uri & uriScheme_ .~ x) === uri { uriScheme = x })
  , testProperty "uriAuthority_ Lens" $ \uri x ->
     (uri ^. uriAuthority_ === uriAuthority uri) .&&.
     ((uri & uriAuthority_ .~ x) === uri { uriAuthority = x })
  , testProperty "uriPath_ Lens" $ \uri x ->
     (uri ^. uriPath_ === uriPath uri) .&&.
     ((uri & uriPath_ .~ x) === uri { uriPath = x })
  , testProperty "uriQuery_ Lens" $ \uri x ->
     (uri ^. uriQuery_ === uriQuery uri) .&&.
     ((uri & uriQuery_ .~ x) === uri { uriQuery = x })
  , testProperty "uriFragment_ Lens" $ \uri x ->
     (uri ^. uriFragment_ === uriFragment uri) .&&.
     ((uri & uriFragment_ .~ x) === uri { uriFragment = x })

  , testProperty "re uriByteString === serializeURI, irrespective of options" $ \uri (Blind opts) ->
     uri ^. re (uriByteString opts) === (serializeURI uri) ^. to toLazyByteString . strict
  , testProperty "bs ^? uriByteString opts === hush $ serializeURI opts" $ \uri (Blind opts) ->
     let bs = (serializeURI uri) ^. to toLazyByteString . strict
     in (bs ^? (uriByteString opts)) === hush (parseURI opts bs)
  ]



-------------------------------------------------------------------------------
hush :: Either e a -> Maybe a
hush (Right x) = Just x
hush _         = Nothing
