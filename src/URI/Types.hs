{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module URI.Types
    (-- * URI-related types
      Scheme(..)
    , Host(..)
    , Port(..)
    , Authority(..)
    , UserInfo(..)
    , Query(..)
    , URI(..)
    , SchemaError(..)
    , URIParseError(..)
    , URIParserOptions(..)
    ) where

-------------------------------------------------------------------------------
import           Data.ByteString            (ByteString)
import           Data.Monoid
import           Data.SafeCopy
import           Data.Word
import           GHC.Generics               (Generic)
-------------------------------------------------------------------------------


-- | Required first component to referring to a specification for the
-- remainder of the URI's components
newtype Scheme = Scheme { getScheme :: ByteString }
  deriving (Show, Eq, SafeCopy)


-------------------------------------------------------------------------------
newtype Host = Host { getHost :: ByteString }
  deriving (Show, Eq, SafeCopy)

-------------------------------------------------------------------------------
-- | While some libraries have chosen to limit this to a Word16, the
-- spec seems to only specify that the string be comprised of digits.
newtype Port = Port { getPort :: ByteString }
  deriving (Show, Eq, SafeCopy)


-------------------------------------------------------------------------------
data Authority = Authority
   { authorityUserInfo :: Maybe UserInfo
   , authorityHost     :: Host
   , authorityPort     :: Maybe Port -- probably a numeric type
   } deriving (Show, Eq, Generic)


-------------------------------------------------------------------------------
data UserInfo = UserInfo
  { uiUsername :: ByteString
  , uiPassword :: ByteString
  } deriving (Show, Eq, Generic)


-------------------------------------------------------------------------------
newtype Query = Query { getQuery :: [(ByteString, ByteString)] }
              deriving (Show, Eq, Monoid, SafeCopy)


-------------------------------------------------------------------------------
data URI = URI
    { uriScheme    :: Scheme
    , uriAuthority :: Maybe Authority
    , uriPath      :: ByteString
    , uriQuery     :: Query
    , uriFragment  :: Maybe ByteString
    } deriving (Show, Eq, Generic)


-------------------------------------------------------------------------------
-- | Options for the parser. You will probably want to use either
-- "strictURIParserOptions" or "laxURIParserOptions"
data URIParserOptions = URIParserOptions
    { upoValidQueryChar :: Word8 -> Bool
    }


-------------------------------------------------------------------------------
-- | URI Parser
-------------------------------------------------------------------------------
data SchemaError = NonAlphaLeading -- ^ Scheme must start with an alphabet character
                 | InvalidChars    -- ^ Subsequent characters in the schema were invalid
                 | MissingColon    -- ^ Schemas must be followed by a colon
                 deriving (Show, Eq, Read, Generic)


data URIParseError = MalformedScheme SchemaError
                   | MalformedUserInfo
                   | MalformedQuery
                   | MalformedFragment
                   | MalformedHost
                   | MalformedPort
                   | MalformedPath
                   | OtherError String -- ^ Catchall for unpredictable errors
                   deriving (Show, Eq, Generic, Read)
