{-# LANGUAGE RankNTypes #-}
{-|

Module      : URI.ByteString.Lens
Description : Lens interface for the uri-bytestring datatypes
Copyright   : (c) Soostone Inc., 2014-2015
                  Michael Xavier, 2014-2015
License     : BSD3
Maintainer  : michael.xavier@soostone.com
Stability   : experimental

This module provides analogs to Lens, Iso, and Prism over the various
types in this library. These are written in a generic way to avoid a
dependency on any particular lens library. If you want to use this
module in your code, you should be able to use these with a number of
packages including lens and lens-family-core.

-}
module URI.ByteString.Lens
    (-- * Lenses over 'Scheme'
      schemeBS
    -- * Lenses over 'Host'
    , hostBS
    -- * Lenses over 'Port'
    , portNumber
    -- * Lenses over 'Authority'
    , authorityUserInfo_
    , authorityHost_
    , authorityPort_
    -- * Lenses over 'UserInfo'
    , uiUsername_
    , uiPassword_
    -- * Lenses over 'Query'
    , queryPairs
    -- * Lenses over 'URI'
    , uriScheme_
    , uriAuthority_
    , uriPath_
    , uriQuery_
    , uriFragment_
    -- * Lenses over 'URIParserOptions'
    , upoValidQueryChar_
    -- * Prisms
    , uriByteString
    , uriByteStringStrict
    , uriByteStringLax
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder (toLazyByteString)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Profunctor
import           Data.Word
-------------------------------------------------------------------------------
import           URI.ByteString.Internal
import           URI.ByteString.Types
-------------------------------------------------------------------------------


-- | @
-- schemeBS :: Iso' 'Scheme' 'ByteString'
-- @
schemeBS
  :: (Functor f, Profunctor p) =>
     p ByteString (f ByteString) -> p Scheme (f Scheme)
schemeBS = iso getScheme Scheme


-------------------------------------------------------------------------------
-- | @
-- hostBS :: Iso' 'Host' 'ByteString'
-- @
hostBS
  :: (Functor f, Profunctor p) =>
     p ByteString (f ByteString) -> p Host (f Host)
hostBS = iso getHost Host


-------------------------------------------------------------------------------
-- | @
-- portNumber :: Iso' 'Port' 'Int'
-- @
portNumber
  :: (Functor f, Profunctor p) => p Int (f Int) -> p Port (f Port)
portNumber = iso getPort Port


-------------------------------------------------------------------------------
-- | @
-- authorityUserInfo_ :: Lens' 'Authority' ('Maybe' 'UserInfo')
-- @
authorityUserInfo_
  :: Functor f =>
     (Maybe UserInfo -> f (Maybe UserInfo)) -> Authority -> f Authority
authorityUserInfo_ =
  lens authorityUserInfo (\a b -> a { authorityUserInfo = b})


-------------------------------------------------------------------------------
-- | @
-- authorityHost_ :: Lens' 'Authority' 'Host'
-- @
authorityHost_
  :: Functor f => (Host -> f Host) -> Authority -> f Authority
authorityHost_ =
  lens authorityHost (\a b -> a { authorityHost = b})


-------------------------------------------------------------------------------
-- | @
-- authorityPort_ :: Lens' 'Authority' ('Maybe' 'Port')
-- @
authorityPort_
  :: Functor f =>
     (Maybe Port -> f (Maybe Port)) -> Authority -> f Authority
authorityPort_ =
  lens authorityPort (\a b -> a { authorityPort = b})


-------------------------------------------------------------------------------
-- | @
-- uiUsername_ :: Lens' 'UserInfo' 'ByteString'
-- @
uiUsername_
  :: Functor f =>
     (ByteString -> f ByteString) -> UserInfo -> f UserInfo
uiUsername_ =
  lens uiUsername (\a b -> a { uiUsername = b})


-------------------------------------------------------------------------------
-- | @
-- uiPassword_ :: Lens' 'UserInfo' 'ByteString'
-- @
uiPassword_
  :: Functor f =>
     (ByteString -> f ByteString) -> UserInfo -> f UserInfo
uiPassword_ =
  lens uiPassword (\a b -> a { uiPassword = b})


-------------------------------------------------------------------------------
-- | @
-- queryPairs :: Iso' 'Query' [('ByteString', 'ByteString')]
-- @
queryPairs
  :: (Functor f, Profunctor p) =>
     p [(ByteString, ByteString)] (f [(ByteString, ByteString)])
     -> p Query (f Query)
queryPairs = iso getQuery Query


-------------------------------------------------------------------------------
-- | @
-- uriScheme_ :: Lens' 'URI' 'Scheme'
-- @
uriScheme_ :: Functor f => (Scheme -> f Scheme) -> URI -> f URI
uriScheme_ =
  lens uriScheme (\a b -> a { uriScheme = b})


-------------------------------------------------------------------------------
-- | @
-- uriAuthority_ :: Lens' 'URI' ('Maybe' 'Authority')
-- @
uriAuthority_
  :: Functor f =>
     (Maybe Authority -> f (Maybe Authority)) -> URI -> f URI
uriAuthority_ =
  lens uriAuthority (\a b -> a { uriAuthority = b})


-------------------------------------------------------------------------------
-- | @
-- uriPath_ :: Lens' 'URI' 'ByteString'
-- @
uriPath_
  :: Functor f => (ByteString -> f ByteString) -> URI -> f URI
uriPath_ =
  lens uriPath (\a b -> a { uriPath = b})


-------------------------------------------------------------------------------
-- | @
-- uriQuery_ :: Lens' 'URI' 'Query'
-- @
uriQuery_ :: Functor f => (Query -> f Query) -> URI -> f URI
uriQuery_ =
  lens uriQuery (\a b -> a { uriQuery = b})


-------------------------------------------------------------------------------
-- | @
-- uriFragment_ :: Lens' 'URI' ('Maybe' 'ByteString')
-- @
uriFragment_
  :: Functor f =>
     (Maybe ByteString -> f (Maybe ByteString)) -> URI -> f URI
uriFragment_ =
  lens uriFragment (\a b -> a { uriFragment = b})


-------------------------------------------------------------------------------
-- | @
-- upoValidQueryChar_ :: Lens' URIParserOptions (Word8 -> Bool)
-- @
upoValidQueryChar_
  :: Functor f =>
     ((Word8 -> Bool) -> f (Word8 -> Bool))
     -> URIParserOptions -> f URIParserOptions
upoValidQueryChar_ =
  lens upoValidQueryChar (\a b -> a { upoValidQueryChar = b})


-------------------------------------------------------------------------------
-- | @
-- uriByteString :: 'URIParserOptions' -> Prism' 'ByteString' 'URI'
-- @
uriByteString
  :: (Applicative f, Choice p) =>
     URIParserOptions -> p URI (f URI) -> p ByteString (f ByteString)
uriByteString opts = prism serialize deserialize
  where
    serialize = toStrict . toLazyByteString . serializeURI
    deserialize s = case parseURI opts s of
      Left _  -> Left s
      Right x -> Right x


-------------------------------------------------------------------------------
-- | @
-- uriByteStringStrict :: Prism' 'ByteString' 'URI'
-- @
uriByteStringStrict
  :: (Applicative f, Choice p) =>
     p URI (f URI) -> p ByteString (f ByteString)
uriByteStringStrict = uriByteString strictURIParserOptions


-------------------------------------------------------------------------------
-- | @
-- uriByteStringLax :: Prism' 'ByteString' 'URI'
-- @
uriByteStringLax
  :: (Applicative f, Choice p) =>
     p URI (f URI) -> p ByteString (f ByteString)
uriByteStringLax = uriByteString laxURIParserOptions


-------------------------------------------------------------------------------
-- Lens machinery
-------------------------------------------------------------------------------
type Lens s t a b = Functor f => (a -> f b) -> s -> f t
type Iso s t a b = (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Prism s t a b = (Choice p, Applicative f) => p a (f b) -> p s (f t)


-------------------------------------------------------------------------------
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}


-------------------------------------------------------------------------------
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}


-------------------------------------------------------------------------------
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}
