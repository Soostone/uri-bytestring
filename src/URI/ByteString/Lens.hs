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
    , authorityUserInfo
    , authorityHost
    , authorityPort
    -- * Lenses over 'UserInfo'
    , uiUsername
    , uiPassword
    -- * Lenses over 'Query'
    , queryPairs
    -- * Lenses over 'URI'
    , uriScheme
    , uriAuthority
    , uriPath
    , uriQuery
    , uriFragment
    -- * Lenses over 'URIParserOptions'
    , upoValidQueryChar
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString         (ByteString)
import           Data.Word
-------------------------------------------------------------------------------
import           URI.ByteString.Types
-------------------------------------------------------------------------------


-- | @
-- schemeBS :: Lens' 'Scheme' 'ByteString'
-- @
schemeBS
  :: Functor f => (ByteString -> f ByteString) -> Scheme -> f Scheme
schemeBS =
  lens _schemeBS (\a b -> a { _schemeBS = b})
{-# INLINE schemeBS #-}

-------------------------------------------------------------------------------
-- | @
-- hostBS :: Lens' 'Host' 'ByteString'
-- @
hostBS
  :: Functor f => (ByteString -> f ByteString) -> Host -> f Host
hostBS =
  lens _hostBS (\a b -> a { _hostBS = b})
{-# INLINE hostBS #-}


-------------------------------------------------------------------------------
-- | @
-- portNumber :: Lens' 'Port' 'Int'
-- @
portNumber
  :: Functor f => (Int -> f Int) -> Port -> f Port
portNumber =
  lens _portNumber (\a b -> a { _portNumber = b})
{-# INLINE portNumber #-}


-------------------------------------------------------------------------------
-- | @
-- authorityUserInfo :: Lens' 'Authority' ('Maybe' 'UserInfo')
-- @
authorityUserInfo
  :: Functor f =>
     (Maybe UserInfo -> f (Maybe UserInfo)) -> Authority -> f Authority
authorityUserInfo =
  lens _authorityUserInfo (\a b -> a { _authorityUserInfo = b})
{-# INLINE authorityUserInfo #-}

-------------------------------------------------------------------------------
-- | @
-- authorityHost :: Lens' 'Authority' 'Host'
-- @
authorityHost
  :: Functor f => (Host -> f Host) -> Authority -> f Authority
authorityHost =
  lens _authorityHost (\a b -> a { _authorityHost = b})
{-# INLINE authorityHost #-}

-------------------------------------------------------------------------------
-- | @
-- authorityPort :: Lens' 'Authority' ('Maybe' 'Port')
-- @
authorityPort
  :: Functor f =>
     (Maybe Port -> f (Maybe Port)) -> Authority -> f Authority
authorityPort =
  lens _authorityPort (\a b -> a { _authorityPort = b})
{-# INLINE authorityPort #-}

-------------------------------------------------------------------------------
-- | @
-- uiUsername :: Lens' 'UserInfo' 'ByteString'
-- @
uiUsername
  :: Functor f =>
     (ByteString -> f ByteString) -> UserInfo -> f UserInfo
uiUsername =
  lens _uiUsername (\a b -> a { _uiUsername = b})
{-# INLINE uiUsername #-}


-------------------------------------------------------------------------------
-- | @
-- uiPassword :: Lens' 'UserInfo' 'ByteString'
-- @
uiPassword
  :: Functor f =>
     (ByteString -> f ByteString) -> UserInfo -> f UserInfo
uiPassword =
  lens _uiPassword (\a b -> a { _uiPassword = b})
{-# INLINE uiPassword #-}


-------------------------------------------------------------------------------
-- | @
-- queryPairs :: Lens' 'Query' [('ByteString', 'ByteString')]
-- @
queryPairs
  :: Functor f
  => ([(ByteString, ByteString)] -> f [(ByteString, ByteString)])
  -> Query
  -> f Query
queryPairs =
  lens _queryPairs (\a b -> a { _queryPairs = b})
{-# INLINE queryPairs #-}


-------------------------------------------------------------------------------
-- | @
-- uriScheme :: Lens' 'URI' 'Scheme'
-- @
uriScheme :: Functor f => (Scheme -> f Scheme) -> URI -> f URI
uriScheme =
  lens _uriScheme (\a b -> a { _uriScheme = b})
{-# INLINE uriScheme #-}


-------------------------------------------------------------------------------
-- | @
-- uriAuthority :: Lens' 'URI' ('Maybe' 'Authority')
-- @
uriAuthority
  :: Functor f =>
     (Maybe Authority -> f (Maybe Authority)) -> URI -> f URI
uriAuthority =
  lens _uriAuthority (\a b -> a { _uriAuthority = b})
{-# INLINE uriAuthority #-}


-------------------------------------------------------------------------------
-- | @
-- uriPath :: Lens' 'URI' 'ByteString'
-- @
uriPath
  :: Functor f => (ByteString -> f ByteString) -> URI -> f URI
uriPath =
  lens _uriPath (\a b -> a { _uriPath = b})
{-# INLINE uriPath #-}


-------------------------------------------------------------------------------
-- | @
-- uriQuery :: Lens' 'URI' 'Query'
-- @
uriQuery :: Functor f => (Query -> f Query) -> URI -> f URI
uriQuery =
  lens _uriQuery (\a b -> a { _uriQuery = b})
{-# INLINE uriQuery #-}

-------------------------------------------------------------------------------
-- | @
-- uriFragment :: Lens' 'URI' ('Maybe' 'ByteString')
-- @
uriFragment
  :: Functor f =>
     (Maybe ByteString -> f (Maybe ByteString)) -> URI -> f URI
uriFragment =
  lens _uriFragment (\a b -> a { _uriFragment = b})
{-# INLINE uriFragment #-}


-------------------------------------------------------------------------------
-- | @
-- upoValidQueryChar :: Lens' URIParserOptions (Word8 -> Bool)
-- @
upoValidQueryChar
  :: Functor f =>
     ((Word8 -> Bool) -> f (Word8 -> Bool))
     -> URIParserOptions -> f URIParserOptions
upoValidQueryChar =
  lens _upoValidQueryChar (\a b -> a { _upoValidQueryChar = b})
{-# INLINE upoValidQueryChar #-}


-------------------------------------------------------------------------------
-- Lens machinery
-------------------------------------------------------------------------------
type Lens s t a b = Functor f => (a -> f b) -> s -> f t

-------------------------------------------------------------------------------
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}
