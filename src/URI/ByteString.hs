{-|

Module      : URI.ByteString
Description : ByteString URI Parser and Serializer
Copyright   : (c) Soostone Inc., 2014-2015
                  Michael Xavier, 2014-2015
License     : BSD3
Maintainer  : michael.xavier@soostone.com
Stability   : experimental

URI.ByteString aims to be an RFC3986 compliant URI parser that uses
efficient ByteStrings for parsing and representing the data. This
module provides a URI datatype as well as a parser and serializer.

Note that this library is an early release and may have issues. It is
currently being used in production and no issues have been
encountered, however. Please report any issues encountered to the
issue tracker.

This module also provides analogs to Lens over the various types in
this library. These are written in a generic way to avoid a dependency
on any particular lens library. You should be able to use these with a
number of packages including lens and lens-family-core.

-}
module URI.ByteString
    (-- * URI-related types
      Scheme(..)
    , Host(..)
    , Port(..)
    , Authority(..)
    , UserInfo(..)
    , Query(..)
    , URIRef(..)
    , Absolute
    , Relative
    , SchemaError(..)
    , URIParseError(..)
    , URIParserOptions(..)
    , strictURIParserOptions
    , laxURIParserOptions
    , URINormalizationOptions(..)
    , noNormalization
    , rfc3986Normalization
    , httpNormalization
    , aggressiveNormalization
    , httpDefaultPorts
    -- * Operations
    , toAbsolute
    -- * Parsing
    , parseURI
    , parseRelativeRef
    , uriParser
    , relativeRefParser
    -- * Serializing
    , serializeURIRef
    , serializeURIRef'
    , serializeQuery
    , serializeQuery'
    , serializeFragment
    , serializeFragment'
    , serializeAuthority
    , serializeAuthority'
    , serializeUserInfo
    , serializeUserInfo'
    -- ** Normalized Serialization
    , normalizeURIRef
    , normalizeURIRef'
    -- * Low level utility functions
    , urlDecode
    , urlDecodeQuery
    , urlEncodeQuery
    , urlEncodePath
    , urlEncode
    -- * Lenses
    -- ** Lenses over 'Scheme'
    , schemeBSL
    -- ** Lenses over 'Host'
    , hostBSL
    -- ** Lenses over 'Port'
    , portNumberL
    -- ** Lenses over 'Authority'
    , authorityUserInfoL
    , authorityHostL
    , authorityPortL
    -- ** Lenses over 'UserInfo'
    , uiUsernameL
    , uiPasswordL
    -- ** Lenses over 'Query'
    , queryPairsL
    -- ** Lenses over 'URIRef'
    , uriSchemeL
    , authorityL
    , pathL
    , queryL
    , fragmentL
    -- ** Lenses over 'URIParserOptions'
    , upoValidQueryCharL
    -- ** Deprecated
    , URI
    , RelativeRef
    , serializeURI
    , serializeURI'
    , serializeRelativeRef
    , serializeRelativeRef'
    , uriAuthorityL
    , uriPathL
    , uriQueryL
    , uriFragmentL
    , rrAuthorityL
    , rrPathL
    , rrQueryL
    , rrFragmentL
    ) where

-------------------------------------------------------------------------------
import           URI.ByteString.Internal
import           URI.ByteString.Lens
import           URI.ByteString.Types
-------------------------------------------------------------------------------
