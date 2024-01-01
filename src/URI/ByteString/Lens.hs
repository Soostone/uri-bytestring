{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module URI.ByteString.Lens where

-------------------------------------------------------------------------------
import Control.Applicative
import Data.ByteString (ByteString)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
import URI.ByteString.Types
import Prelude

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
schemeBSL ::
  Lens' Scheme ByteString
schemeBSL =
  lens schemeBS (\a b -> a {schemeBS = b})
{-# INLINE schemeBSL #-}

-------------------------------------------------------------------------------
hostBSL ::
  Lens' Host ByteString
hostBSL =
  lens hostBS (\a b -> a {hostBS = b})
{-# INLINE hostBSL #-}

-------------------------------------------------------------------------------
portNumberL ::
  Lens' Port Int
portNumberL =
  lens portNumber (\a b -> a {portNumber = b})
{-# INLINE portNumberL #-}

-------------------------------------------------------------------------------
authorityUserInfoL ::
  Lens' Authority (Maybe UserInfo)
authorityUserInfoL =
  lens authorityUserInfo (\a b -> a {authorityUserInfo = b})
{-# INLINE authorityUserInfoL #-}

-------------------------------------------------------------------------------
authorityHostL ::
  Lens' Authority Host
authorityHostL =
  lens authorityHost (\a b -> a {authorityHost = b})
{-# INLINE authorityHostL #-}

-------------------------------------------------------------------------------
authorityPortL ::
  Lens' Authority (Maybe Port)
authorityPortL =
  lens authorityPort (\a b -> a {authorityPort = b})
{-# INLINE authorityPortL #-}

-------------------------------------------------------------------------------
uiUsernameL ::
  Lens' UserInfo ByteString
uiUsernameL =
  lens uiUsername (\a b -> a {uiUsername = b})
{-# INLINE uiUsernameL #-}

-------------------------------------------------------------------------------
uiPasswordL ::
  Lens' UserInfo ByteString
uiPasswordL =
  lens uiPassword (\a b -> a {uiPassword = b})
{-# INLINE uiPasswordL #-}

-------------------------------------------------------------------------------
queryPairsL ::
  Lens' Query [(ByteString, ByteString)]
queryPairsL =
  lens queryPairs (\a b -> a {queryPairs = b})
{-# INLINE queryPairsL #-}

-------------------------------------------------------------------------------
uriAuthorityL :: Lens' URI (Maybe Authority)
uriAuthorityL =
  lens uriAuthority (\a b -> a {uriAuthority = b})
{-# INLINE uriAuthorityL #-}
{-# DEPRECATED uriAuthorityL "Use 'authorityL' instead" #-}

-------------------------------------------------------------------------------
uriPathL :: Lens' URI ByteString
uriPathL =
  lens uriPath (\a b -> a {uriPath = b})
{-# INLINE uriPathL #-}
{-# DEPRECATED uriPathL "Use 'pathL' instead" #-}

-------------------------------------------------------------------------------
uriQueryL :: Lens' URI Query
uriQueryL =
  lens uriQuery (\a b -> a {uriQuery = b})
{-# INLINE uriQueryL #-}
{-# DEPRECATED uriQueryL "Use 'queryL' instead" #-}

-------------------------------------------------------------------------------
uriFragmentL :: Lens' URI (Maybe ByteString)
uriFragmentL =
  lens uriFragment (\a b -> a {uriFragment = b})
{-# INLINE uriFragmentL #-}
{-# DEPRECATED uriFragmentL "Use 'fragmentL' instead" #-}

-------------------------------------------------------------------------------
rrAuthorityL :: Lens' RelativeRef (Maybe Authority)
rrAuthorityL =
  lens rrAuthority (\a b -> a {rrAuthority = b})
{-# INLINE rrAuthorityL #-}
{-# DEPRECATED rrAuthorityL "Use 'authorityL' instead" #-}

-------------------------------------------------------------------------------
rrPathL :: Lens' RelativeRef ByteString
rrPathL =
  lens rrPath (\a b -> a {rrPath = b})
{-# INLINE rrPathL #-}
{-# DEPRECATED rrPathL "Use 'pathL' instead" #-}

-------------------------------------------------------------------------------
rrQueryL :: Lens' RelativeRef Query
rrQueryL =
  lens rrQuery (\a b -> a {rrQuery = b})
{-# INLINE rrQueryL #-}
{-# DEPRECATED rrQueryL "Use 'queryL' instead" #-}

-------------------------------------------------------------------------------
rrFragmentL :: Lens' RelativeRef (Maybe ByteString)
rrFragmentL =
  lens rrFragment (\a b -> a {rrFragment = b})
{-# INLINE rrFragmentL #-}
{-# DEPRECATED rrFragmentL "Use 'fragmentL' instead" #-}

-------------------------------------------------------------------------------
uriSchemeL :: Lens' (URIRef Absolute) Scheme
uriSchemeL = lens uriScheme setter
  where
    setter :: URIRef Absolute -> Scheme -> URIRef Absolute
    setter (URI _ b c d e) a' = URI a' b c d e
{-# INLINE uriSchemeL #-}

-------------------------------------------------------------------------------
authorityL :: Lens' (URIRef a) (Maybe Authority)
authorityL = lens getter setter
  where
    getter :: URIRef a -> Maybe Authority
    getter (URI {..}) = uriAuthority
    getter (RelativeRef {..}) = rrAuthority
    setter :: URIRef a -> Maybe Authority -> URIRef a
    setter (URI a _ c d e) b' = URI a b' c d e
    setter (RelativeRef _ c d e) b' = RelativeRef b' c d e
{-# INLINE authorityL #-}

-------------------------------------------------------------------------------
pathL :: Lens' (URIRef a) ByteString
pathL = lens getter setter
  where
    getter :: URIRef a -> ByteString
    getter (URI {..}) = uriPath
    getter (RelativeRef {..}) = rrPath
    setter :: URIRef a -> ByteString -> URIRef a
    setter (URI a b _ d e) c' = URI a b c' d e
    setter (RelativeRef b _ d e) c' = RelativeRef b c' d e
{-# INLINE pathL #-}

-------------------------------------------------------------------------------
queryL :: Lens' (URIRef a) Query
queryL = lens getter setter
  where
    getter :: URIRef a -> Query
    getter (URI {..}) = uriQuery
    getter (RelativeRef {..}) = rrQuery
    setter :: URIRef a -> Query -> URIRef a
    setter (URI a b c _ e) d' = URI a b c d' e
    setter (RelativeRef b c _ e) d' = RelativeRef b c d' e
{-# INLINE queryL #-}

-------------------------------------------------------------------------------
fragmentL :: Lens' (URIRef a) (Maybe ByteString)
fragmentL = lens getter setter
  where
    getter :: URIRef a -> Maybe ByteString
    getter (URI {..}) = uriFragment
    getter (RelativeRef {..}) = rrFragment
    setter :: URIRef a -> Maybe ByteString -> URIRef a
    setter (URI a b c d _) e' = URI a b c d e'
    setter (RelativeRef b c d _) e' = RelativeRef b c d e'
{-# INLINE fragmentL #-}

-------------------------------------------------------------------------------
upoValidQueryCharL :: Lens' URIParserOptions Bool
upoValidQueryCharL =
  lens upoLaxQueryParsing (\a b -> a {upoLaxQueryParsing = b})
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
