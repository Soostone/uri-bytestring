{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module URI.ByteString.Types where

-------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
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
newtype Query = Query { queryPairs :: [(ByteString, ByteString)] }
              deriving (Show, Eq, Monoid, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
data URI = URI {
      uriScheme    :: Scheme
    , uriAuthority :: Maybe Authority
    , uriPath      :: ByteString
    , uriQuery     :: Query
    , uriFragment  :: Maybe ByteString
    -- ^ URI fragment. Does not include the #
    } deriving (Show, Eq, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
data RelativeRef = RelativeRef {
      rrAuthority :: Maybe Authority
    , rrPath      :: ByteString
    , rrQuery     :: Query
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
