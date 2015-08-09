{-# LANGUAGE RankNTypes #-}
module URI.ByteString.Lens where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Word
-------------------------------------------------------------------------------
import           Prelude
-------------------------------------------------------------------------------
import           URI.ByteString.Types
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
schemeBSL
  :: Lens' Scheme ByteString
schemeBSL =
  lens schemeBS (\a b -> a { schemeBS = b})
{-# INLINE schemeBSL #-}

-------------------------------------------------------------------------------
hostBSL
  :: Lens' Host ByteString
hostBSL =
  lens hostBS (\a b -> a { hostBS = b})
{-# INLINE hostBSL #-}


-------------------------------------------------------------------------------
portNumberL
  :: Lens' Port Int
portNumberL =
  lens portNumber (\a b -> a { portNumber = b})
{-# INLINE portNumberL #-}


-------------------------------------------------------------------------------
authorityUserInfoL
  :: Lens' Authority (Maybe UserInfo)
authorityUserInfoL =
  lens authorityUserInfo (\a b -> a { authorityUserInfo = b})
{-# INLINE authorityUserInfoL #-}

-------------------------------------------------------------------------------
authorityHostL
  :: Lens' Authority Host
authorityHostL =
  lens authorityHost (\a b -> a { authorityHost = b})
{-# INLINE authorityHostL #-}

-------------------------------------------------------------------------------
authorityPortL
  :: Lens' Authority (Maybe Port)
authorityPortL =
  lens authorityPort (\a b -> a { authorityPort = b})
{-# INLINE authorityPortL #-}

-------------------------------------------------------------------------------
uiUsernameL
  :: Lens' UserInfo ByteString
uiUsernameL =
  lens uiUsername (\a b -> a { uiUsername = b})
{-# INLINE uiUsernameL #-}


-------------------------------------------------------------------------------
uiPasswordL
  :: Lens' UserInfo ByteString
uiPasswordL =
  lens uiPassword (\a b -> a { uiPassword = b})
{-# INLINE uiPasswordL #-}


-------------------------------------------------------------------------------
queryPairsL
  :: Lens' Query [(ByteString, ByteString)]
queryPairsL =
  lens queryPairs (\a b -> a { queryPairs = b})
{-# INLINE queryPairsL #-}


-------------------------------------------------------------------------------
uriSchemeL :: Lens' URI Scheme
uriSchemeL =
  lens uriScheme (\a b -> a { uriScheme = b})
{-# INLINE uriSchemeL #-}


-------------------------------------------------------------------------------
uriAuthorityL :: Lens' URI (Maybe Authority)
uriAuthorityL =
  lens uriAuthority (\a b -> a { uriAuthority = b})
{-# INLINE uriAuthorityL #-}


-------------------------------------------------------------------------------
uriPathL :: Lens' URI ByteString
uriPathL =
  lens uriPath (\a b -> a { uriPath = b})
{-# INLINE uriPathL #-}


-------------------------------------------------------------------------------
uriQueryL :: Lens' URI Query
uriQueryL =
  lens uriQuery (\a b -> a { uriQuery = b})
{-# INLINE uriQueryL #-}


-------------------------------------------------------------------------------
uriFragmentL :: Lens' URI (Maybe ByteString)
uriFragmentL =
  lens uriFragment (\a b -> a { uriFragment = b})
{-# INLINE uriFragmentL #-}


-------------------------------------------------------------------------------
rrAuthorityL :: Lens' RelativeRef (Maybe Authority)
rrAuthorityL =
  lens rrAuthority (\a b -> a { rrAuthority = b})
{-# INLINE rrAuthorityL #-}


-------------------------------------------------------------------------------
rrPathL :: Lens' RelativeRef ByteString
rrPathL =
  lens rrPath (\a b -> a { rrPath = b})
{-# INLINE rrPathL #-}


-------------------------------------------------------------------------------
rrQueryL :: Lens' RelativeRef Query
rrQueryL =
  lens rrQuery (\a b -> a { rrQuery = b})
{-# INLINE rrQueryL #-}


-------------------------------------------------------------------------------
rrFragmentL :: Lens' RelativeRef (Maybe ByteString)
rrFragmentL =
  lens rrFragment (\a b -> a { rrFragment = b})
{-# INLINE rrFragmentL #-}


-------------------------------------------------------------------------------
upoValidQueryCharL :: Lens' URIParserOptions (Word8 -> Bool)
upoValidQueryCharL =
  lens upoValidQueryChar (\a b -> a { upoValidQueryChar = b})
{-# INLINE upoValidQueryCharL #-}


-------------------------------------------------------------------------------
-- Lens machinery
-------------------------------------------------------------------------------
-- Unexported type aliases to clean up the documentation
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a


-------------------------------------------------------------------------------
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}
