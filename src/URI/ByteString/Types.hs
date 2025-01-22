{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

#ifdef LIFT_COMPAT
{-# LANGUAGE TemplateHaskell            #-}
#else
{-# LANGUAGE DeriveLift                 #-}
#endif
module URI.ByteString.Types where

-------------------------------------------------------------------------------
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup as Semigroup
import Data.Typeable
import GHC.Generics
import Instances.TH.Lift ()
-------------------------------------------------------------------------------
import Prelude

-------------------------------------------------------------------------------
#ifdef LIFT_COMPAT
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Syntax ()
#else
import           Language.Haskell.TH.Syntax
#endif

-- | Required first component to referring to a specification for the
-- remainder of the URI's components, e.g. "http" or "https"
newtype Scheme = Scheme {schemeBS :: ByteString}
  deriving (Show, Eq, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''Scheme
#else
deriving instance Lift Scheme
#endif

-------------------------------------------------------------------------------
newtype Host = Host {hostBS :: ByteString}
  deriving (Show, Eq, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''Host
#else
deriving instance Lift Host
#endif

-------------------------------------------------------------------------------

-- | While some libraries have chosen to limit this to a Word16, the
-- spec only specifies that the string be comprised of digits.
newtype Port = Port {portNumber :: Int}
  deriving (Show, Eq, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''Port
#else
deriving instance Lift Port
#endif

-------------------------------------------------------------------------------
data UserInfo = UserInfo
  { uiUsername :: ByteString,
    uiPassword :: ByteString
  }
  deriving (Show, Eq, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''UserInfo
#else
deriving instance Lift UserInfo
#endif

-------------------------------------------------------------------------------
data Authority = Authority
  { authorityUserInfo :: Maybe UserInfo,
    authorityHost :: Host,
    authorityPort :: Maybe Port
  }
  deriving (Show, Eq, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''Authority
#else
deriving instance Lift Authority
#endif

-------------------------------------------------------------------------------
newtype Query = Query {queryPairs :: [(ByteString, ByteString)]}
  deriving (Show, Eq, Semigroup.Semigroup, Monoid, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''Query
#else
deriving instance Lift Query
#endif

-------------------------------------------------------------------------------
data Absolute deriving (Typeable)

#ifdef LIFT_COMPAT
deriveLift ''Absolute
#else
deriving instance Lift Absolute
#endif

-------------------------------------------------------------------------------
data Relative deriving (Typeable)

#ifdef LIFT_COMPAT
deriveLift ''Relative
#else
deriving instance Lift Relative
#endif

-------------------------------------------------------------------------------

-- | Note: URI fragment does not include the #
data URIRef a where
  URI ::
    { uriScheme :: Scheme,
      uriAuthority :: Maybe Authority,
      uriPath :: ByteString,
      uriQuery :: Query,
      uriFragment :: Maybe ByteString
    } ->
    URIRef Absolute
  RelativeRef ::
    { rrAuthority :: Maybe Authority,
      rrPath :: ByteString,
      rrQuery :: Query,
      rrFragment :: Maybe ByteString
    } ->
    URIRef Relative

deriving instance Show (URIRef a)

deriving instance Eq (URIRef a)

-- deriving instance Generic (URIRef a)
deriving instance Ord (URIRef a)

#ifdef LIFT_COMPAT
deriveLift ''URIRef
#else
deriving instance Lift (URIRef a)
#endif

#ifdef WITH_TYPEABLE
deriving instance Typeable URIRef
#endif

-------------------------------------------------------------------------------
type URI = URIRef Absolute

-------------------------------------------------------------------------------
type RelativeRef = URIRef Relative

-------------------------------------------------------------------------------

-- | Options for the parser. You will probably want to use either
-- "strictURIParserOptions" or "laxURIParserOptions"
data URIParserOptions = URIParserOptions
  { upoLaxQueryParsing :: Bool
  }

-------------------------------------------------------------------------------
data URINormalizationOptions = URINormalizationOptions
  { -- | hTtP -> http
    unoDowncaseScheme :: Bool,
    -- | eXaMpLe.org -> example.org
    unoDowncaseHost :: Bool,
    -- | If the scheme is known and the port is the default (e.g. 80 for http) it is removed.
    unoDropDefPort :: Bool,
    -- | If the path is empty, set it to \/
    unoSlashEmptyPath :: Bool,
    -- | Rewrite path from \/foo\/\/bar\/\/\/baz to \/foo\/bar\/baz
    unoDropExtraSlashes :: Bool,
    -- | Sorts parameters by parameter name
    unoSortParameters :: Bool,
    -- | Remove dot segments as per <https://tools.ietf.org/html/rfc3986#section-5.2.4 RFC3986 Section 5.2.4>
    unoRemoveDotSegments :: Bool,
    -- | Map of known schemes to their default ports. Used when 'unoDropDefPort' is enabled.
    unoDefaultPorts :: M.Map Scheme Port
  }
  deriving (Show, Eq)

-------------------------------------------------------------------------------

-- | URI Parser Types

-------------------------------------------------------------------------------

data SchemaError
  = -- | Scheme must start with an alphabet character
    NonAlphaLeading
  | -- | Subsequent characters in the schema were invalid
    InvalidChars
  | -- | Schemas must be followed by a colon
    MissingColon
  deriving (Show, Eq, Read, Generic, Typeable, Enum, Bounded)

-------------------------------------------------------------------------------
data URIParseError
  = MalformedScheme SchemaError
  | MalformedUserInfo
  | MalformedQuery
  | MalformedFragment
  | MalformedHost
  | MalformedPort
  | MalformedPath
  | -- | Catchall for unpredictable errors
    OtherError String
  deriving (Show, Eq, Generic, Read, Typeable)
