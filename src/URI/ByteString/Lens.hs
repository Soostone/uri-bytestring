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


-- | @
-- schemeBSL :: Lens' 'Scheme' 'ByteString'
-- @
schemeBSL
  :: Functor f => (ByteString -> f ByteString) -> Scheme -> f Scheme
schemeBSL =
  lens schemeBS (\a b -> a { schemeBS = b})
{-# INLINE schemeBSL #-}

-------------------------------------------------------------------------------
-- | @
-- hostBSL :: Lens' 'Host' 'ByteString'
-- @
hostBSL
  :: Functor f => (ByteString -> f ByteString) -> Host -> f Host
hostBSL =
  lens hostBS (\a b -> a { hostBS = b})
{-# INLINE hostBSL #-}


-------------------------------------------------------------------------------
-- | @
-- portNumberL :: Lens' 'Port' 'Int'
-- @
portNumberL
  :: Functor f => (Int -> f Int) -> Port -> f Port
portNumberL =
  lens portNumber (\a b -> a { portNumber = b})
{-# INLINE portNumberL #-}


-------------------------------------------------------------------------------
-- | @
-- authorityUserInfoL :: Lens' 'Authority' ('Maybe' 'UserInfo')
-- @
authorityUserInfoL
  :: Functor f =>
     (Maybe UserInfo -> f (Maybe UserInfo)) -> Authority -> f Authority
authorityUserInfoL =
  lens authorityUserInfo (\a b -> a { authorityUserInfo = b})
{-# INLINE authorityUserInfoL #-}

-------------------------------------------------------------------------------
-- | @
-- authorityHostL :: Lens' 'Authority' 'Host'
-- @
authorityHostL
  :: Functor f => (Host -> f Host) -> Authority -> f Authority
authorityHostL =
  lens authorityHost (\a b -> a { authorityHost = b})
{-# INLINE authorityHostL #-}

-------------------------------------------------------------------------------
-- | @
-- authorityPortL :: Lens' 'Authority' ('Maybe' 'Port')
-- @
authorityPortL
  :: Functor f =>
     (Maybe Port -> f (Maybe Port)) -> Authority -> f Authority
authorityPortL =
  lens authorityPort (\a b -> a { authorityPort = b})
{-# INLINE authorityPortL #-}

-------------------------------------------------------------------------------
-- | @
-- uiUsernameL :: Lens' 'UserInfo' 'ByteString'
-- @
uiUsernameL
  :: Functor f =>
     (ByteString -> f ByteString) -> UserInfo -> f UserInfo
uiUsernameL =
  lens uiUsername (\a b -> a { uiUsername = b})
{-# INLINE uiUsernameL #-}


-------------------------------------------------------------------------------
-- | @
-- uiPasswordL :: Lens' 'UserInfo' 'ByteString'
-- @
uiPasswordL
  :: Functor f =>
     (ByteString -> f ByteString) -> UserInfo -> f UserInfo
uiPasswordL =
  lens uiPassword (\a b -> a { uiPassword = b})
{-# INLINE uiPasswordL #-}


-------------------------------------------------------------------------------
-- | @
-- uriSchemeL :: Lens' 'URI' 'Scheme'
-- @
uriSchemeL :: Functor f => (Scheme -> f Scheme) -> URI -> f URI
uriSchemeL =
  lens uriScheme (\a b -> a { uriScheme = b})
{-# INLINE uriSchemeL #-}


-------------------------------------------------------------------------------
-- | @
-- uriAuthorityL :: Lens' 'URI' ('Maybe' 'Authority')
-- @
uriAuthorityL
  :: Functor f =>
     (Maybe Authority -> f (Maybe Authority)) -> URI -> f URI
uriAuthorityL =
  lens uriAuthority (\a b -> a { uriAuthority = b})
{-# INLINE uriAuthorityL #-}


-------------------------------------------------------------------------------
-- | @
-- uriPathL :: IsPathSegment a => Lens' 'URI' [a]
-- @
uriPathL
  :: (Functor f, IsPathSegment a) => ([a] -> f [a]) -> URI -> f URI
uriPathL = undefined
{-# INLINE uriPathL #-}


-------------------------------------------------------------------------------
-- | @
-- uriQueryL :: IsQueryPair a => Lens' 'URI''[a]
-- @
uriQueryL :: (Functor f, IsQueryPair a) => ([a] -> f [a]) -> URI -> f URI
uriQueryL = undefined
{-# INLINE uriQueryL #-}


-------------------------------------------------------------------------------
-- | @
-- uriFragmentL :: Lens' 'URI' ('Maybe' 'ByteString')
-- @
uriFragmentL
  :: Functor f =>
     (Maybe ByteString -> f (Maybe ByteString)) -> URI -> f URI
uriFragmentL =
  lens uriFragment (\a b -> a { uriFragment = b})
{-# INLINE uriFragmentL #-}


-------------------------------------------------------------------------------
-- | @
-- rrAuthorityL :: Lens' 'RelativeRef' ('Maybe' 'Authority')
-- @
rrAuthorityL
  :: Functor f =>
     (Maybe Authority -> f (Maybe Authority)) -> RelativeRef -> f RelativeRef
rrAuthorityL =
  lens rrAuthority (\a b -> a { rrAuthority = b})
{-# INLINE rrAuthorityL #-}


-------------------------------------------------------------------------------
-- | @
-- rrPathL :: IsPathSegment a => Lens' 'RelativeRef' [a]
-- @
rrPathL
  :: (Functor f, IsPathSegment a) => ([a] -> f [a]) -> RelativeRef -> f RelativeRef
rrPathL = undefined
{-# INLINE rrPathL #-}


-------------------------------------------------------------------------------
-- | @
-- rrQueryL :: IsQueryPair a => Lens' 'RelativeRef' [a]
-- @
rrQueryL :: (Functor f, IsQueryPair a) => ([a] -> f [a]) -> RelativeRef -> f RelativeRef
rrQueryL = undefined
{-# INLINE rrQueryL #-}


-------------------------------------------------------------------------------
-- | @
-- rrFragmentL :: Lens' 'RelativeRef' ('Maybe' 'ByteString')
-- @
rrFragmentL
  :: Functor f =>
     (Maybe ByteString -> f (Maybe ByteString)) -> RelativeRef -> f RelativeRef
rrFragmentL =
  lens rrFragment (\a b -> a { rrFragment = b})
{-# INLINE rrFragmentL #-}


-------------------------------------------------------------------------------
-- | @
-- upoValidQueryCharL :: Lens' URIParserOptions (Word8 -> Bool)
-- @
upoValidQueryCharL
  :: Functor f =>
     ((Word8 -> Bool) -> f (Word8 -> Bool))
     -> URIParserOptions -> f URIParserOptions
upoValidQueryCharL =
  lens upoValidQueryChar (\a b -> a { upoValidQueryChar = b})
{-# INLINE upoValidQueryCharL #-}


-------------------------------------------------------------------------------
-- Lens machinery
-------------------------------------------------------------------------------
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-------------------------------------------------------------------------------
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}
