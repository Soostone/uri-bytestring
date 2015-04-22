module URI.ByteString.LensTests
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Lens
import           Test.Tasty
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           URI.ByteString
import           URI.ByteString.Arbitrary ()
import           URI.ByteString.Lens
-------------------------------------------------------------------------------



tests :: TestTree
tests = testGroup "URI.ByteString.Lens"
  [
    testProperty "schemeBS Lens" $ \bs bs' ->
      let wrapped = Scheme bs
      in (wrapped ^. schemeBS) === _schemeBS wrapped .&&.
         (wrapped & schemeBS .~ bs') === wrapped { _schemeBS = bs'}
  , testProperty "hostBS Lens" $ \bs bs' ->
      let wrapped = Host bs
      in (wrapped ^. hostBS) === _hostBS wrapped .&&.
         (wrapped & hostBS .~ bs') === wrapped { _hostBS = bs'}
  , testProperty "portNumber Lens" $ \n n' ->
      let wrapped = Port n
      in (wrapped ^. portNumber) === _portNumber wrapped .&&.
         (wrapped & portNumber .~ n') === wrapped { _portNumber = n'}
  , testProperty "queryPairs Lens" $ \ps ps' ->
      let wrapped = Query ps
      in wrapped ^. queryPairs === _queryPairs wrapped .&&.
         (wrapped & queryPairs .~ ps') === wrapped { _queryPairs = ps'}

  , testProperty "authorityUserInfo Lens" $ \a ui ->
     (a ^. authorityUserInfo === _authorityUserInfo a) .&&.
     ((a & authorityUserInfo .~ ui) === a { _authorityUserInfo = ui })
  , testProperty "authorityHost Lens" $ \a host ->
     (a ^. authorityHost === _authorityHost a) .&&.
     ((a & authorityHost .~ host) === a { _authorityHost = host })
  , testProperty "authorityPort Lens" $ \a port ->
     (a ^. authorityPort === _authorityPort a) .&&.
     ((a & authorityPort .~ port) === a { _authorityPort = port })

  , testProperty "uiUsername Lens" $ \ui bs ->
     (ui ^. uiUsername === _uiUsername ui) .&&.
     ((ui & uiUsername .~ bs) === ui { _uiUsername = bs })
  , testProperty "uiPassword Lens" $ \ui bs ->
     (ui ^. uiPassword === _uiPassword ui) .&&.
     ((ui & uiPassword .~ bs) === ui { _uiPassword = bs })

  , testProperty "uriScheme Lens" $ \uri x ->
     (uri ^. uriScheme === _uriScheme uri) .&&.
     ((uri & uriScheme .~ x) === uri { _uriScheme = x })
  , testProperty "uriAuthority Lens" $ \uri x ->
     (uri ^. uriAuthority === _uriAuthority uri) .&&.
     ((uri & uriAuthority .~ x) === uri { _uriAuthority = x })
  , testProperty "uriPath Lens" $ \uri x ->
     (uri ^. uriPath === _uriPath uri) .&&.
     ((uri & uriPath .~ x) === uri { _uriPath = x })
  , testProperty "uriQuery Lens" $ \uri x ->
     (uri ^. uriQuery === _uriQuery uri) .&&.
     ((uri & uriQuery .~ x) === uri { _uriQuery = x })
  , testProperty "uriFragment Lens" $ \uri x ->
     (uri ^. uriFragment === _uriFragment uri) .&&.
     ((uri & uriFragment .~ x) === uri { _uriFragment = x })
  ]
