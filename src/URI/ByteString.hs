{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-|

Module      : URI.ByteString
Description : ByteString URI Parser and Serializer
Copyright   : (c) Soostone Inc., 2014
                  Michael Xavier, 2014
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
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as A
import           Data.Bits
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import           Data.Char                  (ord)
import           Data.Ix
import           Data.List                  (delete, stripPrefix)
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable
import           Data.Word
import           GHC.Generics               (Generic)
import           Text.Read                  (readMaybe)
-------------------------------------------------------------------------------


-- | Required first component to referring to a specification for the
-- remainder of the URI's components, e.g. "http" or "https"
newtype Scheme = Scheme { getScheme :: ByteString }
  deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
newtype Host = Host { getHost :: ByteString }
  deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
-- | While some libraries have chosen to limit this to a Word16, the
-- spec only specifies that the string be comprised of digits.
newtype Port = Port { getPort :: Int }
  deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
data Authority = Authority {
      authorityUserInfo :: Maybe UserInfo
    , authorityHost     :: Host
    , authorityPort     :: Maybe Port
    } deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
data UserInfo = UserInfo {
      uiUsername :: ByteString
    , uiPassword :: ByteString
    } deriving (Show, Eq, Generic, Typeable)


-------------------------------------------------------------------------------
newtype Query = Query { getQuery :: [(ByteString, ByteString)] }
              deriving (Show, Eq, Monoid)


-------------------------------------------------------------------------------
data URI = URI {
      uriScheme    :: Scheme
    , uriAuthority :: Maybe Authority
    , uriPath      :: ByteString
    , uriQuery     :: Query
    , uriFragment  :: Maybe ByteString
    -- ^ URI fragment. Does not include the #
    } deriving (Show, Eq, Generic, Typeable)



-------------------------------------------------------------------------------
-- | Options for the parser. You will probably want to use either
-- "strictURIParserOptions" or "laxURIParserOptions"
data URIParserOptions = URIParserOptions {
      upoValidQueryChar :: Word8 -> Bool
    }


-------------------------------------------------------------------------------
-- | Strict URI Parser config. Follows RFC3986 as-specified. Use this
-- if you can be certain that your URIs are properly encoded or if you
-- want parsing to fail if they deviate from the spec at all.
strictURIParserOptions :: URIParserOptions
strictURIParserOptions =  URIParserOptions {
      upoValidQueryChar = validForQuery
    }


-------------------------------------------------------------------------------
-- | Lax URI Parser config. Use this if you you want to handle common
-- deviations from the spec gracefully.
--
-- * Allows non-encoded [ and ] in query string
laxURIParserOptions :: URIParserOptions
laxURIParserOptions = URIParserOptions {
      upoValidQueryChar = validForQueryLax
    }

-------------------------------------------------------------------------------
-- | URI Serializer
-------------------------------------------------------------------------------

-- | Serialize a URI into a strict ByteString
-- Example:
--
-- >>> serializeURI $ URI {uriScheme = Scheme {getScheme = "http"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {getHost = "www.example.org"}, authorityPort = Nothing}), uriPath = "/foo", uriQuery = Query {getQuery = [("bar","baz")]}, uriFragment = Just "quux"}
-- "http://www.example.org/foo?bar=baz#quux"
serializeURI :: URI -> ByteString
serializeURI URI {..} = scheme <> "://" <>
                        authority <>
                        path <>
                        query <>
                        fragment
  where
    path = BS.intercalate (BS.singleton slash) $ map urlEncodePath segs
    segs = BS.split slash uriPath
    scheme = getScheme uriScheme
    authority = maybe mempty serializeAuthority uriAuthority
    query = serializeQuery uriQuery
    fragment = maybe mempty ("#" <>) uriFragment


-------------------------------------------------------------------------------
serializeQuery :: Query -> ByteString
serializeQuery (Query []) = mempty
serializeQuery (Query ps) = "?" <> BS.intercalate "&" (map serializePair ps)
  where
    serializePair (k, v) = urlEncodeQuery k <> "=" <> urlEncodeQuery v


-------------------------------------------------------------------------------
serializeAuthority :: Authority -> ByteString
serializeAuthority Authority {..} = userinfo <> host <> port
  where
    userinfo = maybe mempty serializeUserInfo authorityUserInfo
    host = getHost authorityHost
    port = maybe mempty (BS8.cons ':'. BS8.pack . show . getPort) authorityPort


-------------------------------------------------------------------------------
serializeUserInfo :: UserInfo -> ByteString
serializeUserInfo UserInfo {..} = uiUsername <> ":" <> uiPassword


-------------------------------------------------------------------------------
-- | URI Parser
-------------------------------------------------------------------------------


data SchemaError = NonAlphaLeading -- ^ Scheme must start with an alphabet character
                 | InvalidChars    -- ^ Subsequent characters in the schema were invalid
                 | MissingColon    -- ^ Schemas must be followed by a colon
                 deriving (Show, Eq, Read, Generic, Typeable)


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


-------------------------------------------------------------------------------
-- | Parse a strict ByteString into a URI or an error.
--
-- Example:
--
-- >>> parseURI strictURIParserOptions "http://www.example.org/foo?bar=baz#quux"
-- Right (URI {uriScheme = Scheme {getScheme = "http"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {getHost = "www.example.org"}, authorityPort = Nothing}), uriPath = "/foo", uriQuery = Query {getQuery = [("bar","baz")]}, uriFragment = Just "quux"})
--
-- >>> parseURI strictURIParserOptions "$$$$://badurl.example.org"
-- Left (MalformedScheme NonAlphaLeading)
--
-- There are some urls that you'll encounter which defy the spec, such
-- as those with square brackets in the query string. If you must be
-- able to parse those, you can use "laxURIParserOptions" or specify your own
--
-- >>> parseURI strictURIParserOptions "http://www.example.org/foo?bar[]=baz"
-- Left MalformedQuery
--
-- >>> parseURI laxURIParserOptions "http://www.example.org/foo?bar[]=baz"
-- Right (URI {uriScheme = Scheme {getScheme = "http"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {getHost = "www.example.org"}, authorityPort = Nothing}), uriPath = "/foo", uriQuery = Query {getQuery = [("bar[]","baz")]}, uriFragment = Nothing})
--
-- >>> let myLaxOptions = URIParserOptions { upoValidQueryChar = liftA2 (||) (upoValidQueryChar strictURIParserOptions) (inClass "[]")}
-- >>> parseURI myLaxOptions "http://www.example.org/foo?bar[]=baz"
-- Right (URI {uriScheme = Scheme {getScheme = "http"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {getHost = "www.example.org"}, authorityPort = Nothing}), uriPath = "/foo", uriQuery = Query {getQuery = [("bar[]","baz")]}, uriFragment = Nothing})
parseURI :: URIParserOptions -> ByteString -> Either URIParseError URI
parseURI opts = parseOnly' OtherError (uriParser opts)


-------------------------------------------------------------------------------
-- | Convenience alias for a parser that can return URIParseError
type URIParser = Parser' URIParseError


-------------------------------------------------------------------------------
-- | Toplevel parser for URIs
uriParser :: URIParserOptions -> URIParser URI
uriParser opts = do
  scheme <- schemeParser
  void $ word8 colon `orFailWith` MalformedScheme MissingColon

  (authority, path) <- hierPartParser
  query <- queryParser opts
  frag  <- mFragmentParser
  case frag of
    Just _ -> endOfInput `orFailWith` MalformedFragment
    Nothing -> endOfInput `orFailWith` MalformedQuery
  return $ URI scheme authority path query frag


-------------------------------------------------------------------------------
-- | Parser for scheme, e.g. "http", "https", etc.
schemeParser :: URIParser Scheme
schemeParser = do
  c    <- satisfy isAlpha           `orFailWith` MalformedScheme NonAlphaLeading
  rest <- A.takeWhile isSchemeValid `orFailWith` MalformedScheme InvalidChars
  return $ Scheme $ c `BS.cons` rest
  where
    isSchemeValid = inClass $ "-+." ++ alphaNum


-------------------------------------------------------------------------------
-- | Hier part immediately follows the schema and encompasses the
-- authority and path sections.
hierPartParser :: URIParser (Maybe Authority, ByteString)
hierPartParser = authWithPathParser <|>
                 pathAbsoluteParser <|>
                 pathRootlessParser <|>
                 pathEmptyParser


-------------------------------------------------------------------------------
-- | See the "authority path-abempty" grammar in the RFC
authWithPathParser :: URIParser (Maybe Authority, ByteString)
authWithPathParser = string' "//" *> ((,) <$> mAuthorityParser <*> pathParser)


-------------------------------------------------------------------------------
-- | See the "path-absolute" grammar in the RFC. Essentially a special
-- case of rootless.
pathAbsoluteParser :: URIParser (Maybe Authority, ByteString)
pathAbsoluteParser = string' "/" *> pathRootlessParser


-------------------------------------------------------------------------------
-- | See the "path-rootless" grammar in the RFC.
pathRootlessParser :: URIParser (Maybe Authority, ByteString)
pathRootlessParser = (,) <$> pure Nothing <*> pathParser1


-------------------------------------------------------------------------------
-- | See the "path-empty" grammar in the RFC. Must not be followed
-- with a path-valid char.
pathEmptyParser :: URIParser (Maybe Authority, ByteString)
pathEmptyParser = do
  nextChar <- peekWord8 `orFailWith` OtherError "impossible peekWord8 error"
  case nextChar of
    Just c -> guard (notInClass pchar c) >> return emptyCase
    _      -> return emptyCase
  where
    emptyCase = (Nothing, mempty)


-------------------------------------------------------------------------------
-- | Parser whe
mAuthorityParser :: URIParser (Maybe Authority)
mAuthorityParser = mParse authorityParser


-------------------------------------------------------------------------------
-- | Parses the user info section of a URl (i.e. for HTTP Basic
-- Authentication). Note that this will decode any percent-encoded
-- data.
userInfoParser :: URIParser UserInfo
userInfoParser =  (uiTokenParser <* word8 atSym) `orFailWith` MalformedUserInfo
  where
    atSym = 64
    uiTokenParser = do
      ui <- A.takeWhile1 validForUserInfo
      let (user, passWithColon) = BS.break (== colon) $ urlDecode' ui
      let pass = BS.drop 1 passWithColon
      return $ UserInfo user pass
    validForUserInfo = inClass $ pctEncoded ++ subDelims ++ (':' : unreserved)


-------------------------------------------------------------------------------
-- | Authority consists of host and port
authorityParser :: URIParser Authority
authorityParser = Authority <$> mParse userInfoParser <*> hostParser <*> mPortParser


-------------------------------------------------------------------------------
-- | Parser that can handle IPV6/Future literals, IPV4, and domain names.
hostParser :: URIParser Host
hostParser = (Host <$> parsers) `orFailWith` MalformedHost
  where
    parsers = ipLiteralParser <|> ipV4Parser <|> regNameParser
    ipLiteralParser = word8 oBracket *> (ipVFutureParser <|> ipV6Parser) <* word8 cBracket


-------------------------------------------------------------------------------
-- | Parses IPV6 addresses. See relevant section in RFC.
ipV6Parser :: Parser ByteString
ipV6Parser = do
    leading <- h16s
    elided <- maybe [] (const [""]) <$> optional (string "::")
    trailing <- many (A.takeWhile (/= colon) <* word8 colon)
    (finalChunkLen, final) <- finalChunk
    let len = length (leading ++ trailing) + finalChunkLen
    when (len > 8) $ fail "Too many digits in IPv6 address"
    return $ rejoin $ [rejoin leading] ++ elided ++ trailing ++ maybeToList final
  where
    finalChunk = fromMaybe (0, Nothing) <$> optional (finalIpV4 <|> finalH16)
    finalH16 = (1, ) . Just <$> h16
    finalIpV4 = (2, ) . Just <$> ipV4Parser
    rejoin = BS.intercalate ":"
    h16s = h16 `sepBy` word8 colon
    h16 = mconcat <$> parseBetween 1 4 (A.takeWhile1 hexDigit)


-------------------------------------------------------------------------------
-- | Parses IPVFuture addresses. See relevant section in RFC.
ipVFutureParser :: Parser ByteString
ipVFutureParser = do
    _    <- word8 lowercaseV
    ds   <- A.takeWhile1 hexDigit
    _    <- word8 period
    rest <- A.takeWhile1 $ inClass $ subDelims ++ ":" ++ unreserved
    return $ "v" <> ds <> "." <> rest
  where
    lowercaseV = 118


-------------------------------------------------------------------------------
-- | Parses a valid IPV4 address
ipV4Parser :: Parser ByteString
ipV4Parser = mconcat <$> sequence [ decOctet
                                  , dot
                                  , decOctet
                                  , dot
                                  , decOctet
                                  , dot
                                  , decOctet]
  where
    decOctet = do
      s <- A.takeWhile1 isDigit
      let len = BS.length s
      guard $ len > 0 && len <= 3
      let num = bsToNum s
      guard $ num >= 1 && num <= 255
      return s
    dot = string "."


-------------------------------------------------------------------------------
-- | This corresponds to the hostname, e.g. www.example.org
regNameParser :: Parser ByteString
regNameParser = urlDecode' <$> A.takeWhile1 (inClass validForRegName)
  where
    validForRegName = pctEncoded ++ subDelims ++ unreserved


-------------------------------------------------------------------------------
-- | Only parse a port if the colon signifier is there.
mPortParser :: URIParser (Maybe Port)
mPortParser = word8' colon `thenJust` portParser


-------------------------------------------------------------------------------
-- | Parses port number from the hostname. Colon separator must be
-- handled elsewhere.
portParser :: URIParser Port
portParser = (Port . bsToNum <$> A.takeWhile1 isDigit) `orFailWith` MalformedPort


-------------------------------------------------------------------------------
-- | Path with any number of segments
pathParser :: URIParser ByteString
pathParser = pathParser' A.many'


-------------------------------------------------------------------------------
-- | Path with at least 1 segment
pathParser1 :: URIParser ByteString
pathParser1 = pathParser' A.many1'


-------------------------------------------------------------------------------
-- | Parses the path section of a url. Note that while this can take
-- percent-encoded characters, it does not itself decode them while parsing.
pathParser' :: (Parser ByteString -> Parser [ByteString]) -> URIParser ByteString
pathParser' repeatParser = (mconcat <$> repeatParser segmentParser) `orFailWith` MalformedPath
  where
    segmentParser = mconcat <$> sequence [string "/", A.takeWhile (inClass pchar)]


-------------------------------------------------------------------------------
-- | This parser is being a bit pragmatic. The query section in the
-- spec does not identify the key/value format used in URIs, but that
-- is what most users are expecting to see. One alternative could be
-- to just expose the query string as a string and offer functions on
-- URI to parse a query string to a Query.
queryParser :: URIParserOptions -> URIParser Query
queryParser opts = do
  mc <- peekWord8 `orFailWith` OtherError "impossible peekWord8 error"
  case mc of
    Just c
      | c == question -> skip' 1 *> itemsParser
      | c == hash     -> pure mempty
      | otherwise     -> fail' MalformedPath
    _      -> pure mempty
  where
    itemsParser = Query <$> A.sepBy' (queryItemParser opts) (word8' ampersand)


-------------------------------------------------------------------------------
-- | When parsing a single query item string like "foo=bar", turns it
-- into a key/value pair as per convention, with the value being
-- optional. & separators need to be handled further up.
queryItemParser :: URIParserOptions -> URIParser (ByteString, ByteString)
queryItemParser opts = do
  s <- A.takeWhile1 (upoValidQueryChar opts) `orFailWith` MalformedQuery
  let (k, vWithEquals) = BS.break (== equals) s
  let v = BS.drop 1 vWithEquals
  return (urlDecodeQuery k, urlDecodeQuery v)


-------------------------------------------------------------------------------
validForQuery :: Word8 -> Bool
validForQuery = inClass ('?':'/':delete '&' pchar)


-------------------------------------------------------------------------------
validForQueryLax :: Word8 -> Bool
validForQueryLax = notInClass "&#"


-------------------------------------------------------------------------------
-- | Only parses a fragment if the # signifiier is there
mFragmentParser :: URIParser (Maybe ByteString)
mFragmentParser = word8' hash `thenJust` fragmentParser


-------------------------------------------------------------------------------
-- | The final piece of a uri, e.g. #fragment, minus the #.
fragmentParser :: URIParser ByteString
fragmentParser = A.takeWhile1 validFragmentWord `orFailWith` MalformedFragment
  where
    validFragmentWord = inClass ('?':'/':pchar)


-------------------------------------------------------------------------------
-- | Grammar Components
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
hexDigit :: Word8 -> Bool
hexDigit = inClass "0-9a-fA-F"


-------------------------------------------------------------------------------
isAlpha :: Word8 -> Bool
isAlpha = inClass alpha


-------------------------------------------------------------------------------
isDigit :: Word8 -> Bool
isDigit = inClass digit


-------------------------------------------------------------------------------
pchar :: String
pchar = pctEncoded ++ subDelims ++ ":@" ++ unreserved


-------------------------------------------------------------------------------
-- Very important!  When concatenating this to other strings to make larger
-- character classes, you must put this at the end because the '-' character
-- is treated as a range unless it's at the beginning or end.
unreserved :: String
unreserved = alphaNum ++ "~._-"


-------------------------------------------------------------------------------
unreserved8 :: [Word8]
unreserved8 = map ord8 unreserved


-------------------------------------------------------------------------------
unreservedPath8 :: [Word8]
unreservedPath8 = unreserved8 ++ map ord8 ":@&=+$,"

-------------------------------------------------------------------------------
ord8 :: Char -> Word8
ord8 = fromIntegral . ord


-------------------------------------------------------------------------------
-- | pc-encoded technically is % HEXDIG HEXDIG but that's handled by
-- the previous alphaNum constraint. May need to double back with a
-- parser to ensure pct-encoded never exceeds 2 hexdigs after
pctEncoded :: String
pctEncoded = "%"


-------------------------------------------------------------------------------
subDelims :: String
subDelims = "!$&'()*+,;="


-------------------------------------------------------------------------------
alphaNum :: String
alphaNum = alpha ++ digit


-------------------------------------------------------------------------------
alpha :: String
alpha = "a-zA-Z"


-------------------------------------------------------------------------------
digit :: String
digit = "0-9"


-------------------------------------------------------------------------------
colon :: Word8
colon = 58


-------------------------------------------------------------------------------
oBracket :: Word8
oBracket = 91


-------------------------------------------------------------------------------
cBracket :: Word8
cBracket = 93


-------------------------------------------------------------------------------
equals :: Word8
equals = 61


-------------------------------------------------------------------------------
question :: Word8
question = 63


-------------------------------------------------------------------------------
ampersand :: Word8
ampersand = 38


-------------------------------------------------------------------------------
hash :: Word8
hash = 35


-------------------------------------------------------------------------------
period :: Word8
period = 46


-------------------------------------------------------------------------------
percent :: Word8
percent = 37

-------------------------------------------------------------------------------
slash :: Word8
slash = 47


-------------------------------------------------------------------------------
-- | ByteString Utilities
-------------------------------------------------------------------------------

-- FIXME: theres probably a much better way to do this

-------------------------------------------------------------------------------
-- | Convert a bytestring into an int representation. Assumes the
-- entire string is comprised of 0-9 digits.
bsToNum :: ByteString -> Int
bsToNum s = sum $ zipWith (*) (reverse ints) [10 ^ x | x <- [0..] :: [Int]]
  where
    w2i w = fromEnum $ w - 48
    ints  = map w2i . BS.unpack $ s


-------------------------------------------------------------------------------
-- | Decoding specifically for the query string, which decodes + as
-- space.
urlDecodeQuery :: ByteString -> ByteString
urlDecodeQuery = urlDecode plusToSpace
  where
    plusToSpace = True


-------------------------------------------------------------------------------
-- | Decode any part of the URL besides the query, which decodes + as
-- space.
urlDecode' :: ByteString -> ByteString
urlDecode' = urlDecode plusToSpace
  where
    plusToSpace = False


-------------------------------------------------------------------------------
-- | Parsing with Strongly-Typed Errors
-------------------------------------------------------------------------------


-- | A parser with a specific error type. Attoparsec unfortunately
-- throws all errors into strings, which cannot be handled well
-- programmatically without doing something silly like parsing error
-- messages. This wrapper attempts to concentrate these errors into
-- one type.
newtype Parser' e a = Parser' (Parser a)
                    deriving ( Functor
                             , Applicative
                             , Alternative
                             , Monad
                             , MonadPlus
                             , Monoid)


-------------------------------------------------------------------------------
-- | Use with caution. Catch a parser failing and return Nothing.
mParse :: Parser' e a -> Parser' e (Maybe a)
mParse p = option Nothing (Just <$> p)


-------------------------------------------------------------------------------
-- | If the first parser succeeds, discard the result and use the
-- second parser (which may fail). If the first parser fails, return
-- Nothing. This is used to check a benign precondition that indicates
-- the presence of a parsible token, i.e. ? preceeding a query.
thenJust :: Parser' e a -> Parser' e b -> Parser' e (Maybe b)
thenJust p1 p2 = p1 *> (Just <$> p2) <|> pure Nothing


-------------------------------------------------------------------------------
-- | Lift a word8 Parser into a strongly error typed parser. This will
-- generate a "stringy" error message if it fails, so you should
-- probably be prepared to exit with a nicer error further up.
word8' :: Word8 -> Parser' e Word8
word8' = Parser' . word8


-------------------------------------------------------------------------------
-- | Skip exactly 1 character. Fails if the character isn't
-- there. Generates a "stringy" error.
skip' :: Int -> Parser' e ()
skip' = Parser' . void . A.take


-------------------------------------------------------------------------------
-- | Lifted version of the string token parser. Same caveats about
-- "stringy" errors apply.
string' :: ByteString -> Parser' e ByteString
string' = Parser' . string


-------------------------------------------------------------------------------
-- | Combinator for tunnelling more specific error types through the
-- attoparsec machinery using read/show.
orFailWith :: (Show e, Read e) => Parser a -> e -> Parser' e a
orFailWith p e = Parser' p <|> fail' e


-------------------------------------------------------------------------------
-- | Should be preferred to fail'
fail' :: (Show e, Read e) => e -> Parser' e a
fail' = fail . show


-------------------------------------------------------------------------------
parseBetween :: (Alternative m, Monad m) => Int -> Int -> m a -> m [a]
parseBetween a b f = choice parsers
  where
    parsers = map (`count` f) $ reverse $ range (a, b)


-------------------------------------------------------------------------------
-- | Stronger-typed variation of parseOnly'. Consumes all input.
parseOnly' :: (Read e, Show e)
              => (String -> e) -- ^ Fallback if we can't parse a failure message for the sake of totality.
              -> Parser' e a
              -> ByteString
              -> Either e a
parseOnly' noParse (Parser' p) = fmapL readWithFallback . parseOnly p
  where
    readWithFallback s = fromMaybe (noParse s) (readMaybe . stripAttoparsecGarbage $ s)

-------------------------------------------------------------------------------
-- | Our pal Control.Monad.fail is how attoparsec propagates
-- errors. If you throw an error string with fail (your only choice),
-- it will *always* prepend it with "Failed reading: ". At least in
-- this version. That may change to something else and break this workaround.
stripAttoparsecGarbage :: String -> String
stripAttoparsecGarbage = stripPrefix' "Failed reading: "


-------------------------------------------------------------------------------
-- | stripPrefix where it is a noop if the prefix doesn't exist.
stripPrefix' :: Eq a => [a] -> [a] -> [a]
stripPrefix' pfx s = fromMaybe s $ stripPrefix pfx s


-------------------------------------------------------------------------------
fmapL :: (a -> b) -> Either a r -> Either b r
fmapL f = either (Left . f) Right


-------------------------------------------------------------------------------
-- | This function was extract from the @http-types@ package. The
-- license can be found in licenses/http-types/LICENSE
urlDecode
    :: Bool -- ^ Whether to decode '+' to ' '
    -> BS.ByteString
    -> BS.ByteString
urlDecode replacePlus z = fst $ BS.unfoldrN (BS.length z) go z
  where
    go bs =
        case BS.uncons bs of
            Nothing -> Nothing
            Just (43, ws) | replacePlus -> Just (32, ws) -- plus to space
            Just (37, ws) -> Just $ fromMaybe (37, ws) $ do -- percent
                (x, xs) <- BS.uncons ws
                x' <- hexVal x
                (y, ys) <- BS.uncons xs
                y' <- hexVal y
                Just (combine x' y', ys)
            Just (w, ws) -> Just (w, ws)
    hexVal w
        | 48 <= w && w <= 57  = Just $ w - 48 -- 0 - 9
        | 65 <= w && w <= 70  = Just $ w - 55 -- A - F
        | 97 <= w && w <= 102 = Just $ w - 87 -- a - f
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b


-------------------------------------------------------------------------------
--TODO: keep an eye on perf here. seems like a good use case for a DList. the word8 list could be a set/hashset
-- | Percent-encoding for URLs.
urlEncode' :: [Word8] -> ByteString -> ByteString
urlEncode' extraUnreserved = BS.pack . mconcat . map encodeChar . BS.unpack
    where
      encodeChar ch | isUnreserved ch = [ch]
                    | otherwise       = pct ch

      isUnreserved ch = inClass alphaNum ch || ch `elem` extraUnreserved

      pct v = let (a, b) = v `divMod` 16 in [percent, pct' a, pct' b]
      pct' i | i < 10    = 48 + i -- zero (0)
             | otherwise = 65 + i - 10 -- 65: A


-------------------------------------------------------------------------------
urlEncodeQuery :: ByteString -> ByteString
urlEncodeQuery = urlEncode' unreserved8


-------------------------------------------------------------------------------
urlEncodePath :: ByteString -> ByteString
urlEncodePath = urlEncode' unreservedPath8
