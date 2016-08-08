{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module URI.ByteString.Types where

-------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Typeable
import           Data.Word
import           GHC.Generics
-------------------------------------------------------------------------------
import           Prelude
-------------------------------------------------------------------------------


-- | Required first component to referring to a specification for the
-- remainder of the URI's components, e.g. "http" or "https"
newtype Scheme = Scheme { schemeBS :: ByteString }
  deriving (Show, Eq, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
newtype Host = Host { hostBS :: ByteString }
  deriving (Show, Eq, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
-- | While some libraries have chosen to limit this to a Word16, the
-- spec only specifies that the string be comprised of digits.
newtype Port = Port { portNumber :: Int }
  deriving (Show, Eq, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
data Authority = Authority {
      authorityUserInfo :: Maybe UserInfo
    , authorityHost     :: Host
    , authorityPort     :: Maybe Port
    } deriving (Show, Eq, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
data UserInfo = UserInfo {
      uiUsername :: ByteString
    , uiPassword :: ByteString
    } deriving (Show, Eq, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
data Query
    = Query { queryPairs :: [(ByteString, ByteString)] }
    | QueryString { queryString :: ByteString }
    | NoQueryString
    deriving (Show, Eq, Generic, Typeable, Ord)

-- The objective here is that Query still acts like a Monoid, especially for
-- the Query constructor. The rest tries to meet the project goal of zero
-- compiler warnings. The handling of NoQueryString (behaving like mempty)
-- seems reasonable. However, mixing Query and QueryString should be considered
-- undefined.
instance Monoid Query where
    mempty = Query []
    (Query a) `mappend` (Query b) = Query $ mappend a b
    (QueryString a) `mappend` (QueryString b) = QueryString $ mappend a b
    NoQueryString `mappend` b = b
    a `mappend` NoQueryString = a
    a `mappend` _ = a

-------------------------------------------------------------------------------
-- | Note: URI fragment does not include the #
data URIRef a where
  URI :: { uriScheme :: Scheme
         , uriAuthority :: Maybe Authority
         , uriPath :: ByteString
         , uriQuery :: Query
         , uriFragment :: Maybe ByteString
         } -> URIRef Absolute
  RelativeRef :: { rrAuthority :: Maybe Authority
                 , rrPath :: ByteString
                 , rrQuery :: Query
                 , rrFragment :: Maybe ByteString
                 } -> URIRef Relative

deriving instance Show (URIRef a)
deriving instance Eq (URIRef a)
-- deriving instance Generic (URIRef a)
deriving instance Ord (URIRef a)

#ifdef WITH_TYPEABLE
deriving instance Typeable URIRef
#endif

-------------------------------------------------------------------------------
data Absolute deriving(Typeable)


-------------------------------------------------------------------------------
data Relative deriving(Typeable)


-------------------------------------------------------------------------------
type URI = URIRef Absolute


-------------------------------------------------------------------------------
type RelativeRef = URIRef Relative


-------------------------------------------------------------------------------
-- | Options for the parser. You will probably want to use either
-- "strictURIParserOptions" or "laxURIParserOptions"
data URIParserOptions = URIParserOptions {
      upoValidQueryChar :: Word8 -> Bool
    , upoParseQuery     :: Bool
    }


-------------------------------------------------------------------------------
data URINormalizationOptions = URINormalizationOptions {
      unoDowncaseScheme    :: Bool
    -- ^ hTtP -> http
    , unoDowncaseHost      :: Bool
    -- ^ eXaMpLe.org -> example.org
    , unoDropDefPort       :: Bool
    -- ^ If the scheme is known and the port is the default (e.g. 80 for http) it is removed.
    , unoSlashEmptyPath    :: Bool
    -- ^ If the path is empty, set it to \/
    , unoDropExtraSlashes  :: Bool
    -- ^ Rewrite path from \/foo\/\/bar\/\/\/baz to \/foo\/bar\/baz
    , unoSortParameters    :: Bool
    -- ^ Sorts parameters by parameter name
    , unoRemoveDotSegments :: Bool
    -- ^ Remove dot segments as per <https://tools.ietf.org/html/rfc3986#section-5.2.4 RFC3986 Section 5.2.4>
    , unoDefaultPorts      :: M.Map Scheme Port
    -- ^ Map of known schemes to their default ports. Used when 'unoDropDefPort' is enabled.
    } deriving (Show, Eq)


-------------------------------------------------------------------------------
-- | URI Parser Types
-------------------------------------------------------------------------------


data SchemaError = NonAlphaLeading -- ^ Scheme must start with an alphabet character
                 | InvalidChars    -- ^ Subsequent characters in the schema were invalid
                 | MissingColon    -- ^ Schemas must be followed by a colon
                 deriving (Show, Eq, Read, Generic, Typeable)


-------------------------------------------------------------------------------
data URIParseError = MalformedScheme SchemaError
                   | MalformedUserInfo
                   | MalformedQuery
                   | MalformedFragment
                   | MalformedHost
                   | MalformedPort
                   | MalformedPath
                   | OtherError String -- ^ Catchall for unpredictable errors
                   deriving (Show, Eq, Generic, Read, Typeable)
