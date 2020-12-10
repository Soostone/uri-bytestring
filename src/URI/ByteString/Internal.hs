{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
module URI.ByteString.Internal where

-------------------------------------------------------------------------------
import           Blaze.ByteString.Builder           (Builder)
import qualified Blaze.ByteString.Builder           as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BB
import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Fail                 as F
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString         as A
import qualified Data.Attoparsec.ByteString.Char8   as A (decimal)
import           Data.Bits
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Char8              as BS8
import           Data.Char                          (ord, toLower)
import           Data.Ix
import           Data.List                          (delete, intersperse,
                                                     sortBy, stripPrefix, (\\))
import qualified Data.Map.Strict                    as M
import           Data.Maybe
import           Data.Monoid                        as Monoid (mempty)
import           Data.Ord                           (comparing)
import           Data.Semigroup                     as Semigroup
import           Data.Word
import           Text.Read                          (readMaybe)
-------------------------------------------------------------------------------
import           URI.ByteString.Types
-------------------------------------------------------------------------------


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
-- | All normalization options disabled
noNormalization :: URINormalizationOptions
noNormalization = URINormalizationOptions False False False False False False False httpDefaultPorts


-------------------------------------------------------------------------------
-- | The set of known default ports to schemes. Currently only
-- contains http\/80 and https\/443. Feel free to extend it if needed
-- with 'unoDefaultPorts'.
httpDefaultPorts :: M.Map Scheme Port
httpDefaultPorts = M.fromList [ (Scheme "http", Port 80)
                              , (Scheme "https", Port 443)
                              ]


-------------------------------------------------------------------------------
-- | Only normalizations deemed appropriate for all protocols by
-- RFC3986 enabled, namely:
--
-- * Downcase Scheme
-- * Downcase Host
-- * Remove Dot Segments
rfc3986Normalization :: URINormalizationOptions
rfc3986Normalization = noNormalization { unoDowncaseScheme = True
                                       , unoDowncaseHost = True
                                       , unoRemoveDotSegments = True
                                       }


-------------------------------------------------------------------------------
-- | The same as 'rfc3986Normalization' but with additional enabled
-- features if you're working with HTTP URIs:
--
-- * Drop Default Port (with 'httpDefaultPorts')
-- * Drop Extra Slashes
httpNormalization :: URINormalizationOptions
httpNormalization = rfc3986Normalization { unoDropDefPort = True
                                         , unoSlashEmptyPath = True
                                         }

-------------------------------------------------------------------------------
-- | All options enabled
aggressiveNormalization :: URINormalizationOptions
aggressiveNormalization = URINormalizationOptions True True True True True True True httpDefaultPorts


-------------------------------------------------------------------------------
-- | @toAbsolute scheme ref@ converts @ref@ to an absolute URI.
-- If @ref@ is already absolute, then it is unchanged.
toAbsolute :: Scheme -> URIRef a -> URIRef Absolute
toAbsolute scheme (RelativeRef {..}) = URI scheme rrAuthority rrPath rrQuery rrFragment
toAbsolute _ uri@(URI {..}) = uri


-------------------------------------------------------------------------------
-- | URI Serializer
-------------------------------------------------------------------------------

-- | Serialize a URI reference into a 'Builder'.
--
-- Example of serializing + converting to a lazy "Data.ByteString.Lazy.ByteString":
--
-- >>> BB.toLazyByteString $ serializeURIRef $ URI {uriScheme = Scheme {schemeBS = "http"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "www.example.org"}, authorityPort = Nothing}), uriPath = "/foo", uriQuery = Query {queryPairs = [("bar","baz")]}, uriFragment = Just "quux"}
-- "http://www.example.org/foo?bar=baz#quux"
serializeURIRef :: URIRef a -> Builder
serializeURIRef = normalizeURIRef noNormalization


-------------------------------------------------------------------------------
-- | Like 'serializeURIRef', with conversion into a strict 'ByteString'.
serializeURIRef' :: URIRef a -> ByteString
serializeURIRef' = BB.toByteString . serializeURIRef


-------------------------------------------------------------------------------
-- | Serialize a URI into a Builder.
serializeURI :: URIRef Absolute -> Builder
serializeURI = normalizeURIRef noNormalization
{-# DEPRECATED serializeURI "Use 'serializeURIRef' instead" #-}


-------------------------------------------------------------------------------
-- | Similar to 'serializeURIRef' but performs configurable degrees of
-- URI normalization. If your goal is the fastest serialization speed
-- possible, 'serializeURIRef' will be fine. If you intend on
-- comparing URIs (say for caching purposes), you'll want to use this.
normalizeURIRef :: URINormalizationOptions -> URIRef a -> Builder
normalizeURIRef o uri@(URI {..})       = normalizeURI o uri
normalizeURIRef o uri@(RelativeRef {}) = normalizeRelativeRef o Nothing uri


-------------------------------------------------------------------------------
normalizeURIRef' :: URINormalizationOptions -> URIRef a -> ByteString
normalizeURIRef' o = BB.toByteString . normalizeURIRef o


-------------------------------------------------------------------------------
normalizeURI :: URINormalizationOptions -> URIRef Absolute -> Builder
normalizeURI o@URINormalizationOptions {..} URI {..} =
  scheme <> BB.fromString ":" <> normalizeRelativeRef o (Just uriScheme) rr
  where
    scheme = bs (sCase (schemeBS uriScheme))
    sCase
      | unoDowncaseScheme = downcaseBS
      | otherwise = id
    rr = RelativeRef uriAuthority uriPath uriQuery uriFragment


-------------------------------------------------------------------------------
normalizeRelativeRef :: URINormalizationOptions -> Maybe Scheme -> URIRef Relative -> Builder
normalizeRelativeRef o@URINormalizationOptions {..} mScheme RelativeRef {..} =
  authority <> path <> query <> fragment
  where
    path
      | unoSlashEmptyPath && BS.null rrPath  = BB.fromByteString "/"
      | segs == [""] = BB.fromByteString "/"
      | otherwise  = mconcat (intersperse (c8 '/') (map urlEncodePath segs))
    segs = dropSegs (BS.split slash (pathRewrite rrPath))
    pathRewrite
      | unoRemoveDotSegments = removeDotSegments
      | otherwise = id
    dropSegs [] = []
    dropSegs (h:t)
      | unoDropExtraSlashes = h:(filter (not . BS.null) t)
      | otherwise = h:t
    authority = maybe Monoid.mempty (serializeAuthority o mScheme) rrAuthority
    query = serializeQuery o rrQuery
    fragment = serializeFragment rrFragment


-------------------------------------------------------------------------------
--TODO: this is probably ripe for benchmarking
-- | Algorithm described in
-- <https://tools.ietf.org/html/rfc3986#section-5.2.4>, reproduced
-- artlessly.
removeDotSegments :: ByteString -> ByteString
removeDotSegments path = mconcat (rl2L (go path (RL [])))
  where
    go inBuf outBuf
      -- A. If the input buffer begins with prefix of ../ or ./ then
      -- remove the prefix from the input buffer
      | BS8.isPrefixOf "../" inBuf = go (BS8.drop 3 inBuf) outBuf
      | BS8.isPrefixOf "./" inBuf  = go (BS8.drop 2 inBuf) outBuf
      -- B. If the input buffer begins with a prefix of "/./" or "/.",
      -- where "." is a complete path segment, then replace that
      -- prefix with "/" in the input buffer. TODO: I think "a
      -- complete path segment" means its the whole thing?
      | BS.isPrefixOf "/./" inBuf = go (BS8.drop 2 inBuf) outBuf
      | inBuf == "/." = go "/" outBuf
      -- C. If the input buffer begins with a prefix of "/../" or
      -- "/..", where ".." is a complete path segment, then replace
      -- that prefix with "/" in the input buffer and remove the last
      -- segment and its preceding "/" (if any) from the output buffer
      | BS.isPrefixOf "/../" inBuf = go (BS8.drop 3 inBuf) (unsnoc (unsnoc outBuf))
      | inBuf == "/.." = go "/" (unsnoc (unsnoc outBuf))
      -- D. If the input buffer consists only of "." or "..", then
      -- remove that from the input buffer
      | inBuf == "." = go mempty outBuf
      | inBuf == ".." = go mempty outBuf
      -- E. Move the first path segment in the input buffer to the end
      -- of the output buffer, including the initial "/" character (if
      -- any) and any subsequent characters up to, but not including,
      -- the next "/" character or the end of the input buffer.
      | otherwise = case BS8.uncons inBuf of
                      Just ('/', rest) ->
                        let (thisSeg, inBuf') = BS8.span (/= '/') rest
                        in go inBuf' (outBuf |> "/" |> thisSeg)
                      Just (_, _) ->
                        let (thisSeg, inBuf') = BS8.span (/= '/') inBuf
                        in go inBuf' (outBuf |> thisSeg)
                      Nothing -> outBuf



-------------------------------------------------------------------------------
-- | Like 'serializeURI', with conversion into a strict 'ByteString'.
serializeURI' :: URIRef Absolute -> ByteString
serializeURI' = BB.toByteString . serializeURI
{-# DEPRECATED serializeURI' "Use 'serializeURIRef'' instead" #-}


-------------------------------------------------------------------------------
-- | Like 'serializeURI', but do not render scheme.
serializeRelativeRef :: URIRef Relative -> Builder
serializeRelativeRef = normalizeRelativeRef noNormalization Nothing
{-# DEPRECATED serializeRelativeRef "Use 'serializeURIRef' instead" #-}


-------------------------------------------------------------------------------
-- | Like 'serializeRelativeRef', with conversion into a strict 'ByteString'.
serializeRelativeRef' :: URIRef Relative -> ByteString
serializeRelativeRef' = BB.toByteString . serializeRelativeRef
{-# DEPRECATED serializeRelativeRef' "Use 'serializeURIRef'' instead" #-}


-------------------------------------------------------------------------------
-- | Serialize the query part of a url
-- @serializeQuery opts mempty = ""@
-- @serializeQuery opts (Query [("a","b"),("c","d")]) = "?a=b&c=d"@
serializeQuery :: URINormalizationOptions -> Query -> Builder
serializeQuery _ (Query []) = mempty
serializeQuery URINormalizationOptions {..} (Query ps) =
    c8 '?' <> mconcat (intersperse (c8 '&') (map serializePair ps'))
  where
    serializePair (k, v) = urlEncodeQuery k <> c8 '=' <> urlEncodeQuery v
    ps'
      | unoSortParameters = sortBy (comparing fst) ps
      | otherwise = ps


serializeQuery' :: URINormalizationOptions -> Query -> ByteString
serializeQuery' opts = BB.toByteString . serializeQuery opts


-------------------------------------------------------------------------------
serializeFragment :: Maybe ByteString -> Builder
serializeFragment = maybe mempty (\s -> c8 '#' <> bs s)


serializeFragment' :: Maybe ByteString -> ByteString
serializeFragment' = BB.toByteString . serializeFragment


-------------------------------------------------------------------------------
serializeAuthority :: URINormalizationOptions -> Maybe Scheme -> Authority -> Builder
serializeAuthority URINormalizationOptions {..} mScheme Authority {..} = BB.fromString "//" <> userinfo <> bs host <> port
  where
    userinfo = maybe mempty serializeUserInfo authorityUserInfo
    host = hCase (hostBS authorityHost)
    hCase
      | unoDowncaseHost = downcaseBS
      | otherwise = id
    port = maybe mempty packPort effectivePort
    effectivePort = do
      p <- authorityPort
      dropPort mScheme p
    packPort (Port p) = c8 ':' <> BB.fromString (show p)
    dropPort Nothing = Just
    dropPort (Just scheme)
      | unoDropDefPort = dropPort' scheme
      | otherwise = Just
    dropPort' s p
      | M.lookup s unoDefaultPorts == Just p = Nothing
      | otherwise = Just p


serializeAuthority' :: URINormalizationOptions -> Maybe Scheme -> Authority -> ByteString
serializeAuthority' opts mScheme = BB.toByteString . serializeAuthority opts mScheme

-------------------------------------------------------------------------------
serializeUserInfo :: UserInfo -> Builder
serializeUserInfo UserInfo {..} = bs uiUsername <> c8 ':' <> bs uiPassword <> c8 '@'


serializeUserInfo' :: UserInfo -> ByteString
serializeUserInfo' = BB.toByteString . serializeUserInfo


-------------------------------------------------------------------------------
bs :: ByteString -> Builder
bs = BB.fromByteString


-------------------------------------------------------------------------------
c8 :: Char -> Builder
c8 = BB.fromChar


-------------------------------------------------------------------------------
-- | Parse a strict ByteString into a URI or an error.
--
-- Example:
--
-- >>> parseURI strictURIParserOptions "http://www.example.org/foo?bar=baz#quux"
-- Right (URI {uriScheme = Scheme {schemeBS = "http"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "www.example.org"}, authorityPort = Nothing}), uriPath = "/foo", uriQuery = Query {queryPairs = [("bar","baz")]}, uriFragment = Just "quux"})
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
-- Right (URI {uriScheme = Scheme {schemeBS = "http"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "www.example.org"}, authorityPort = Nothing}), uriPath = "/foo", uriQuery = Query {queryPairs = [("bar[]","baz")]}, uriFragment = Nothing})
--
-- >>> let myLaxOptions = URIParserOptions { upoValidQueryChar = liftA2 (||) (upoValidQueryChar strictURIParserOptions) (inClass "[]")}
-- >>> parseURI myLaxOptions "http://www.example.org/foo?bar[]=baz"
-- Right (URI {uriScheme = Scheme {schemeBS = "http"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "www.example.org"}, authorityPort = Nothing}), uriPath = "/foo", uriQuery = Query {queryPairs = [("bar[]","baz")]}, uriFragment = Nothing})
parseURI :: URIParserOptions -> ByteString -> Either URIParseError (URIRef Absolute)
parseURI opts = parseOnly' OtherError (uriParser' opts)

-- | Like 'parseURI', but do not parse scheme.
parseRelativeRef :: URIParserOptions -> ByteString -> Either URIParseError (URIRef Relative)
parseRelativeRef opts = parseOnly' OtherError (relativeRefParser' opts)


-------------------------------------------------------------------------------
-- | Convenience alias for a parser that can return URIParseError
type URIParser = Parser' URIParseError


-------------------------------------------------------------------------------
-- | Underlying attoparsec parser. Useful for composing with your own parsers.
uriParser :: URIParserOptions -> Parser (URIRef Absolute)
uriParser = unParser' . uriParser'


-------------------------------------------------------------------------------
-- | Toplevel parser for URIs
uriParser' :: URIParserOptions -> URIParser (URIRef Absolute)
uriParser' opts = do
  scheme <- schemeParser
  void $ word8 colon `orFailWith` MalformedScheme MissingColon
  RelativeRef authority path query fragment <- relativeRefParser' opts
  return $ URI scheme authority path query fragment


-------------------------------------------------------------------------------
-- | Underlying attoparsec parser. Useful for composing with your own parsers.
relativeRefParser :: URIParserOptions -> Parser (URIRef Relative)
relativeRefParser = unParser' . relativeRefParser'


-------------------------------------------------------------------------------
-- | Toplevel parser for relative refs
relativeRefParser' :: URIParserOptions -> URIParser (URIRef Relative)
relativeRefParser' opts = do
  (authority, path) <- hierPartParser <|> rrPathParser
  query <- queryParser opts
  frag  <- mFragmentParser
  case frag of
    Just _  -> endOfInput `orFailWith` MalformedFragment
    Nothing -> endOfInput `orFailWith` MalformedQuery
  return $ RelativeRef authority path query frag


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
-- | Relative references have awkward corner cases.  See
-- 'firstRelRefSegmentParser'.
rrPathParser :: URIParser (Maybe Authority, ByteString)
rrPathParser = (Nothing,) <$>
    ((<>) <$> firstRelRefSegmentParser <*> pathParser)


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
-- | Parses the user info section of a URL (i.e. for HTTP Basic
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
    decOctet :: Parser ByteString
    decOctet = do
      (s,num) <- A.match A.decimal
      let len = BS.length s
      guard $ len <= 3
      guard $ num >= (1 :: Int) && num <= 255
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
portParser = (Port <$> A.decimal) `orFailWith` MalformedPort


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
pathParser' repeatParser = (urlDecodeQuery . mconcat <$> repeatParser segmentParser) `orFailWith` MalformedPath
  where
    segmentParser = mconcat <$> sequence [string "/", A.takeWhile (inClass pchar)]


-------------------------------------------------------------------------------
-- | Parses the first segment of a path section of a relative-path
-- reference.  See RFC 3986, Section 4.2.
-- firstRelRefSegmentParser :: URIParser ByteString
firstRelRefSegmentParser :: URIParser ByteString
firstRelRefSegmentParser = A.takeWhile (inClass (pchar \\ ":")) `orFailWith` MalformedPath


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
    itemsParser = Query . filter neQuery <$> A.sepBy' (queryItemParser opts) (word8' ampersand)
    neQuery (k, _) = not (BS.null k)


-------------------------------------------------------------------------------
-- | When parsing a single query item string like "foo=bar", turns it
-- into a key/value pair as per convention, with the value being
-- optional. & separators need to be handled further up.
queryItemParser :: URIParserOptions -> URIParser (ByteString, ByteString)
queryItemParser opts = do
  s <- A.takeWhile (upoValidQueryChar opts) `orFailWith` MalformedQuery
  if BS.null s
     then return (mempty, mempty)
     else do
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
mFragmentParser = mParse $ word8' hash *> fragmentParser


-------------------------------------------------------------------------------
-- | The final piece of a uri, e.g. #fragment, minus the #.
fragmentParser :: URIParser ByteString
fragmentParser = Parser' $ A.takeWhile validFragmentWord
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
slash :: Word8
slash = 47


-------------------------------------------------------------------------------
-- | ByteString Utilities
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Decoding specifically for the query string, which decodes + as
-- space. Shorthand for @urlDecode True@
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
newtype Parser' e a = Parser' { unParser' :: Parser a}
                    deriving ( Functor
                             , Applicative
                             , Alternative
                             , Monad
                             , MonadPlus
                             , Semigroup.Semigroup
                             , Monoid)


instance F.MonadFail (Parser' e) where
#if MIN_VERSION_attoparsec(0,13,1)
  fail e = Parser' (F.fail e)
#else
  fail e = Parser' (fail e)
#endif


-------------------------------------------------------------------------------
-- | Use with caution. Catch a parser failing and return Nothing.
mParse :: Parser' e a -> Parser' e (Maybe a)
mParse p = A.option Nothing (Just <$> p)


-------------------------------------------------------------------------------
-- | If the first parser succeeds, discard the result and use the
-- second parser (which may fail). If the first parser fails, return
-- Nothing. This is used to check a benign precondition that indicates
-- the presence of a parsible token, i.e. ? preceding a query.
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
orFailWith :: (Show e) => Parser a -> e -> Parser' e a
orFailWith p e = Parser' p <|> fail' e


-------------------------------------------------------------------------------
-- | Should be preferred to fail'
fail' :: (Show e) => e -> Parser' e a
fail' = fail . show


-------------------------------------------------------------------------------
parseBetween :: (Alternative m, Monad m) => Int -> Int -> m a -> m [a]
parseBetween a b f = choice parsers
  where
    parsers = map (`count` f) $ reverse $ range (a, b)


-------------------------------------------------------------------------------
-- | Stronger-typed variation of parseOnly'. Consumes all input.
parseOnly' :: (Read e)
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
-- | This function was extracted from the @http-types@ package. The
-- license can be found in licenses/http-types/LICENSE
urlDecode
    :: Bool
    -- ^ Whether to decode '+' to ' '
    -> BS.ByteString
    -> BS.ByteString
urlDecode replacePlus z = fst $ BS.unfoldrN (BS.length z) go z
  where
    go bs' =
        case BS.uncons bs' of
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

-- | Percent-encoding for URLs. Specify a list of additional
-- unreserved characters to permit.
urlEncode :: [Word8] -> ByteString -> Builder
urlEncode extraUnreserved = mconcat . map encodeChar . BS.unpack
    where
      encodeChar ch | unreserved' ch = BB.fromWord8 ch
                    | otherwise     = h2 ch

      unreserved' ch | ch >= 65 && ch <= 90  = True -- A-Z
                    | ch >= 97 && ch <= 122 = True -- a-z
                    | ch >= 48 && ch <= 57  = True -- 0-9
      unreserved' c = c `elem` extraUnreserved

      h2 v = let (a, b) = v `divMod` 16 in bs $ BS.pack [37, h a, h b] -- percent (%)
      h i | i < 10    = 48 + i -- zero (0)
          | otherwise = 65 + i - 10 -- 65: A


-------------------------------------------------------------------------------
-- | Encode a ByteString for use in the query section of a URL
urlEncodeQuery :: ByteString -> Builder
urlEncodeQuery = urlEncode unreserved8


-------------------------------------------------------------------------------
-- | Encode a ByteString for use in the path section of a URL
urlEncodePath :: ByteString -> Builder
urlEncodePath = urlEncode unreservedPath8


-------------------------------------------------------------------------------
downcaseBS :: ByteString -> ByteString
downcaseBS = BS8.map toLower


-------------------------------------------------------------------------------
-- | Simple data structure to get O(1) prepends on a list and defers the O(n)
newtype RL a = RL [a] deriving (Show)


(|>) :: RL a -> a -> RL a
RL as |> a = RL (a:as)


rl2L :: RL a -> [a]
rl2L (RL as) = reverse as


unsnoc :: RL a -> RL a
unsnoc (RL [])     = RL []
unsnoc (RL (_:xs)) = RL xs
