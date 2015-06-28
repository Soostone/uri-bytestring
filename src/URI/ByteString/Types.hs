{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module URI.ByteString.Types where

-------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Data.Typeable
import           Data.Word
import           GHC.Generics
-------------------------------------------------------------------------------
import           Prelude
-------------------------------------------------------------------------------


-- | Required first component to referring to a specification for the
-- remainder of the URI's components, e.g. "http" or "https"
newtype Scheme = Scheme { schemeBS :: ByteString }
  deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
newtype Host = Host { hostBS :: ByteString }
  deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
-- | While some libraries have chosen to limit this to a Word16, the
-- spec only specifies that the string be comprised of digits.
newtype Port = Port { portNumber :: Int }
  deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
data Authority = Authority {
      authorityUserInfo :: Maybe UserInfo
    , authorityHost     :: Host
    , authorityPort     :: Maybe Port
    } deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
data UserInfo = UserInfo {
      uiUsername :: ByteString
    , uiPassword :: ByteString
    } deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
-- | To conform with the standard, 'URI' and 'RelativeRef' store the query as
-- an unparsed 'ByteString'.  In order to process a query, you need to define
-- your own type and instantiate this class.  For example:
--
-- >>> data MyQuery = FilterName ByteString | FilterAge Int | SortFlag
-- >>> instance IsQueryPair MyQuery where
-- >>>    toQueryPair ("name", name)                = Right $ FilterName name
-- >>>    toQueryPair ("age",  readMay -> Just age) = Right $ FilterAge age
-- >>>    toQueryPair ("sort", _)                   = Right $ SortFlag
-- >>>    toQueryPair (k, v) = Left ("bad: " <> k <> ", " <> v)
-- >>>
-- >>>    fromQueryPair (FilterName name) = ("name", name)
-- >>>    fromQueryPair (FilterAge age)   = ("age",  BS.pack . show $ Just age)
-- >>>    fromQueryPair (SortFlag)        = ("sort", "")
--
-- See also: 'IsPathSegment'.
class IsQueryPair a where
    toQueryPair   :: (ByteString, ByteString) -> Either URIParseError a
    fromQueryPair :: a -> (ByteString, ByteString)


-------------------------------------------------------------------------------
-- | Class for mapping bytestring path segments onto custom path segment types.
-- To extend the example from 'IsQueryPair' with matrix query path segments:
--
-- >>> data MySegment = Segment ByteString | MatrixQuery MyQuery
-- >>> instance IsPathSegment MySegment where
-- >>>     toPathSegment s = case parseMatrixQuery s of
-- >>>         Left _  -> Segment s
-- >>>         Right m -> MatrixQuery m
-- >>>
-- >>>     fromPathSegment (Segment s) = s
-- >>>     fromPathSegment (MatrixQuery m) =
-- >>>         BB.toLazyByteString $ serializeMatrixQuery m
class IsPathSegment a where
    toPathSegment   :: ByteString -> Either URIParseError a
    fromPathSegment :: a -> ByteString


-------------------------------------------------------------------------------
data URI = URI {
      uriScheme    :: Scheme
    , uriAuthority :: Maybe Authority
    , uriPath      :: [ByteString]
    , uriQuery     :: ByteString
    , uriFragment  :: Maybe ByteString
    -- ^ URI fragment. Does not include the #
    } deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
data RelativeRef = RelativeRef {
      rrAuthority :: Maybe Authority
    , rrPath      :: [ByteString]
    , rrQuery     :: ByteString
    , rrFragment  :: Maybe ByteString
    -- ^ URI fragment. Does not include the #
    } deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
-- | Options for the parser. You will probably want to use either
-- "strictURIParserOptions" or "laxURIParserOptions"
data URIParserOptions = URIParserOptions {
      upoValidQueryChar :: Word8 -> Bool
    }


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
