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
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString         as A
import           Data.Bits
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import           Data.Char                          (ord)
import           Data.Ix
import           Data.List                          (delete, intersperse,
                                                     stripPrefix, (\\))
import           Data.Maybe
import           Data.Monoid
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
-- | URI Serializer
-------------------------------------------------------------------------------

-- | Serialize a URI into a strict ByteString
-- Example:
--
-- >>> BB.toLazyByteString $ serializeURI $ URI {uriScheme = Scheme {schemeBS = "http"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "www.example.org"}, authorityPort = Nothing}), uriPath = "/foo", uriQuery = Query {queryPairs = [("bar","baz")]}, uriFragment = Just "quux"}
-- "http://www.example.org/foo?bar=baz#quux"
serializeURI :: URI -> Builder
serializeURI URI {..} = scheme <> BB.fromString ":" <>
                        serializeRelativeRef rr
  where
    scheme = bs $ schemeBS uriScheme
    rr = RelativeRef uriAuthority undefined uriQuery uriFragment

-- | Like 'serializeURI', but do not render scheme.
serializeRelativeRef :: RelativeRef -> Builder
serializeRelativeRef RelativeRef {..} = authority <> path <> query <> fragment
  where
    path = mconcat $ intersperse (c8 '/') $ map urlEncodePath segs
    segs = BS.split slash undefined
    authority = maybe mempty serializeAuthority rrAuthority
    query = undefined
    fragment = maybe mempty (\s -> c8 '#' <> bs s) rrFragment

-- | Serialize a query into a ByteString.  Use this to store a query in a URI.
serializeQuery :: IsQueryPair a => [a] -> Builder
serializeQuery = undefined

-- | Serialize a matrix query into a ByteString.  Use this to store a query in a Path segment.
serializeMatrixQuery :: IsQueryPair a => [a] -> Builder
serializeMatrixQuery = undefined


-------------------------------------------------------------------------------
serializeAuthority :: Authority -> Builder
serializeAuthority Authority {..} = BB.fromString "//" <> userinfo <> bs host <> port
  where
    userinfo = maybe mempty serializeUserInfo authorityUserInfo
    host = hostBS authorityHost
    port = maybe mempty packPort authorityPort
    packPort (Port p) = c8 ':' <> BB.fromString (show p)


-------------------------------------------------------------------------------
serializeUserInfo :: UserInfo -> Builder
serializeUserInfo UserInfo {..} = bs uiUsername <> c8 ':' <> bs uiPassword <> c8 '@'


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
parseURI :: URIParserOptions -> ByteString -> Either URIParseError URI
parseURI opts = parseOnly' OtherError (uriParser opts)

-- | Like 'parseURI', but do not parse scheme.
parseRelativeRef :: URIParserOptions -> ByteString -> Either URIParseError RelativeRef
parseRelativeRef opts = parseOnly' OtherError (relativeRefParser opts)

parseQuery :: IsQueryPair a => ByteString -> either URIParseError [a]
parseQuery = undefined

parseMatrixQuery :: IsQueryPair a => ByteString -> either URIParseError [a]
parseMatrixQuery = undefined


-------------------------------------------------------------------------------
-- | Convenience alias for a parser that can return URIParseError
type URIParser = Parser' URIParseError


-------------------------------------------------------------------------------
-- | Toplevel parser for URIs
uriParser :: URIParserOptions -> URIParser URI
uriParser opts = do
  scheme <- schemeParser
  void $ word8 colon `orFailWith` MalformedScheme MissingColon
  RelativeRef authority path query fragment <- relativeRefParser opts
  return $ URI scheme authority path query fragment


-------------------------------------------------------------------------------
-- | Toplevel parser for relative refs
relativeRefParser :: URIParserOptions -> URIParser RelativeRef
relativeRefParser _ = do
  (authority, path) <- undefined
  query <- undefined
  frag  <- mFragmentParser
  case frag of
    Just _ -> endOfInput `orFailWith` MalformedFragment
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
queryParser :: IsQueryPair a => URIParserOptions -> URIParser [a]
queryParser _ = undefined


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
-- | Percent-encoding for URLs.
urlEncode' :: [Word8] -> ByteString -> Builder
urlEncode' extraUnreserved = mconcat . map encodeChar . BS.unpack
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
urlEncodeQuery :: ByteString -> Builder
urlEncodeQuery = urlEncode' unreserved8


-------------------------------------------------------------------------------
urlEncodePath :: ByteString -> Builder
urlEncodePath = urlEncode' unreservedPath8
