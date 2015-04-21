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

-}
module URI.ByteString
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
    , strictURIParserOptions
    , laxURIParserOptions
    -- * Parsing
    , parseURI
    -- * Serializing
    , serializeURI
    ) where

-------------------------------------------------------------------------------
import           URI.ByteString.Internal
import           URI.ByteString.Types
-------------------------------------------------------------------------------
