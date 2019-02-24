{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
#ifdef LIFT_COMPAT
{-# LANGUAGE TemplateHaskell            #-}
#else
{-# LANGUAGE DeriveLift                 #-}
#endif
module URI.ByteString.Types where

-------------------------------------------------------------------------------
import           Control.DeepSeq
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Semigroup (Semigroup)
import           Data.Typeable
import           Data.Word
import           GHC.Generics
import           Instances.TH.Lift()
-------------------------------------------------------------------------------
import           Prelude
-------------------------------------------------------------------------------
#ifdef LIFT_COMPAT
import           Language.Haskell.TH.Syntax()
import           Language.Haskell.TH.Lift
#else
import           Language.Haskell.TH.Syntax
#endif

-- | Required first component to referring to a specification for the
-- remainder of the URI's components, e.g. "http" or "https"
newtype Scheme = Scheme { schemeBS :: ByteString }
  deriving (Show, Eq, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''Scheme
#else
deriving instance Lift Scheme
#endif
instance NFData Scheme

-------------------------------------------------------------------------------
newtype Host = Host { hostBS :: ByteString }
  deriving (Show, Eq, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''Host
#else
deriving instance Lift Host
#endif
instance NFData Host

-------------------------------------------------------------------------------
-- | While some libraries have chosen to limit this to a Word16, the
-- spec only specifies that the string be comprised of digits.
newtype Port = Port { portNumber :: Int }
  deriving (Show, Eq, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''Port
#else
deriving instance Lift Port
#endif
instance NFData Port

-------------------------------------------------------------------------------
data UserInfo = UserInfo {
      uiUsername :: ByteString
    , uiPassword :: ByteString
    } deriving (Show, Eq, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''UserInfo
#else
deriving instance Lift UserInfo
#endif
instance NFData UserInfo

-------------------------------------------------------------------------------
data Authority = Authority {
      authorityUserInfo :: Maybe UserInfo
    , authorityHost     :: Host
    , authorityPort     :: Maybe Port
    } deriving (Show, Eq, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''Authority
#else
deriving instance Lift Authority
#endif
instance NFData Authority

-------------------------------------------------------------------------------
newtype Query = Query { queryPairs :: [(ByteString, ByteString)] }
              deriving (Show, Eq, Semigroup, Monoid, Generic, Typeable, Ord)

#ifdef LIFT_COMPAT
deriveLift ''Query
#else
deriving instance Lift Query
#endif
instance NFData Query

-------------------------------------------------------------------------------
data Absolute deriving(Typeable)

#ifdef LIFT_COMPAT
deriveLift ''Absolute
#else
deriving instance Lift Absolute
#endif

-------------------------------------------------------------------------------
data Relative deriving(Typeable)

#ifdef LIFT_COMPAT
deriveLift ''Relative
#else
deriving instance Lift Relative
#endif

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
#ifdef LIFT_COMPAT
deriveLift ''URIRef
#else
deriving instance Lift (URIRef a)
#endif

#ifdef WITH_TYPEABLE
deriving instance Typeable URIRef
#endif

instance NFData (URIRef a) where
  rnf u@URI{} = (uriScheme u) `deepseq` (uriAuthority u) `deepseq` (uriPath u) `deepseq` (uriFragment u) `deepseq` ()
  rnf u@RelativeRef{} = (rrAuthority u) `deepseq` (rrPath u) `deepseq` (rrQuery u) `deepseq` (rrFragment u) `deepseq` ()

-------------------------------------------------------------------------------
type URI = URIRef Absolute


-------------------------------------------------------------------------------
type RelativeRef = URIRef Relative


-------------------------------------------------------------------------------
-- | Options for the parser. You will probably want to use either
-- "strictURIParserOptions" or "laxURIParserOptions"
data URIParserOptions = URIParserOptions {
      upoValidQueryChar :: Word8 -> Bool
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

instance NFData SchemaError

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

instance NFData URIParseError
