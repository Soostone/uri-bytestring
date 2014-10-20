{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-|
Module      : URI.ByteString
Description : ByteString URI Parser
Copyright   : (c) Soostone Inc., 2014
                  Michael Xavier, 2014
License     : BSD3
Maintainer  : michael@michaelxavier.net
Stability   : experimental
Portability : POSIX

URI.ByteString aims to be an RFC3986 compliant URI parser that uses
efficient ByteStrings for parsing and representing the data.
|-}
module URI.ByteString (
                      -- URI-related types
                        Scheme(..)
                      , Host(..)
                      , Port(..)
                      , Authority(..)
                      , UserInfo(..)
                      , Query(..)
                      , URI(..)
                      , SchemaError(..)
                      , URIParseError(..)

                      -- Parsing
                      , parseURI
                      ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as A
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.List                  (delete, stripPrefix)
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           GHC.Generics               (Generic)
import           Network.HTTP.Types.URI     (urlDecode)
import           Text.Read                  (readMaybe)
-------------------------------------------------------------------------------


------------------------------------------------------------------------------

-- | Required first component to referring to a specification for the
-- remainder of the URI's components
newtype Scheme = Scheme { getScheme :: ByteString }
  deriving (Show, Eq)
newtype Host = Host { getHost :: ByteString }
  deriving (Show, Eq)

-- | While some libraries have chosen to limit this to a Word16, the
-- spec seems to only specify that the string be comprised of digits.
newtype Port = Port { getPort :: ByteString }
  deriving (Show, Eq)

data Authority = Authority
   { authorityUserInfo :: Maybe UserInfo
   , authorityHost     :: Host
   , authorityPort     :: Maybe Port -- probably a numeric type
   } deriving (Show, Eq, Generic)

data UserInfo = UserInfo
  { uiUsername :: ByteString
  , uiPassword :: ByteString
  } deriving (Show, Eq, Generic)

newtype Query = Query { getQuery :: [(ByteString, ByteString)] }
              deriving (Show, Eq, Monoid)

data URI = URI
    { uriScheme    :: Scheme
    , uriAuthority :: Maybe Authority
    , uriPath      :: ByteString
    , uriQuery     :: Query
    , uriFragment  :: Maybe ByteString
    } deriving (Show, Eq, Generic)


                             --------------------
                             -- URI Parser --
                             --------------------

data SchemaError = NonAlphaLeading -- ^ Scheme must start with an alphabet character
                 | InvalidChars    -- ^ Subsequent characters in the schema were invalid
                 | MissingColon    -- ^ Schemas must be followed by a colon
                 deriving (Show, Eq, Read, Generic)


data URIParseError = MalformedScheme SchemaError
                   | MalformedUserInfo
                   | MalformedQuery
                   | MalformedFragment
                   | MalformedHost
                   | MalformedPort
                   | MalformedPath
                   | OtherError String -- ^ Catchall for unpredictable errors
                   deriving (Show, Eq, Generic, Read)


-- | Parse a strict ByteString into a URI or an error.
--
-- Example:
--
-- >>> parseURI "http://www.example.org/foo?bar=baz#quux"
-- Right (URI {uriScheme = Scheme {getScheme = "http"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {getHost = "www.example.org"}, authorityPort = Nothing}), uriPath = "/foo", uriQuery = Query {getQuery = [("bar","baz")]}, uriFragment = Just "quux"})
--
-- >>> parseURI "$$$$://badurl.example.org"
-- Left (MalformedScheme NonAlphaLeading)

parseURI :: ByteString -> Either URIParseError URI
parseURI = parseOnly' OtherError uriParser

-- | Convenience alias for a parser that can return URIParseError
type URIParser = Parser' URIParseError

-- | Toplevel parser for URIs
uriParser :: URIParser URI
uriParser = do
  scheme <- schemeParser
  void $ word8 colon `orFailWith` MalformedScheme MissingColon

  (authority, path) <- hierPartParser
  query <- queryParser
  frag  <- mFragmentParser
  return $ URI scheme authority path query frag

-- | Parser for scheme, e.g. "http", "https", etc.
schemeParser :: URIParser Scheme
schemeParser = do
  c    <- satisfy isAlpha           `orFailWith` MalformedScheme NonAlphaLeading
  rest <- A.takeWhile isSchemeValid `orFailWith` MalformedScheme InvalidChars
  return $ Scheme $ c `BS.cons` rest
  where
    isSchemeValid = inClass $ "-+." ++ alphaNum

--TODO: handle absolute, noscheme, empty

-- | Hier part immediately follows the schema and encompasses the
-- authority and path sections.
hierPartParser :: URIParser (Maybe Authority, ByteString)
hierPartParser = authWithPathParser <|>
                 pathAbsoluteParser <|>
                 pathRootlessParser <|>
                 pathEmptyParser

-- | See the "authority path-abempty" grammar in the RFC
authWithPathParser :: URIParser (Maybe Authority, ByteString)
authWithPathParser = string' "//" *> ((,) <$> mAuthorityParser <*> pathParser)

-- | See the "path-absolute" grammar in the RFC. Essentially a special
-- case of rootless.
pathAbsoluteParser :: URIParser (Maybe Authority, ByteString)
pathAbsoluteParser = string' "/" *> pathRootlessParser

-- | See the "path-rootless" grammar in the RFC.
pathRootlessParser :: URIParser (Maybe Authority, ByteString)
pathRootlessParser = (,) <$> pure Nothing <*> pathParser1

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

-- | Parser whe
mAuthorityParser :: URIParser (Maybe Authority)
mAuthorityParser = mParse authorityParser

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

-- | Authority consists of host and port
authorityParser :: URIParser Authority
authorityParser = Authority <$> mParse userInfoParser <*> hostParser <*> mPortParser

-- | Parser that can handle IPV6/Future literals, IPV4, and domain names.
hostParser :: URIParser Host
hostParser = (Host <$> parsers) `orFailWith` MalformedHost
  where
    parsers = ipLiteralParser <|> ipV4Parser <|> regNameParser
    ipLiteralParser = word8 oBracket *> (ipV6Parser <|> ipVFutureParser) <* word8 cBracket

-- | Parses IPV6 addresses. See relevant section in RFC.
ipV6Parser :: Parser ByteString
ipV6Parser = fail "FIXME"

-- | Parses IPVFuture addresses. See relevant section in RFC.
ipVFutureParser :: Parser ByteString
ipVFutureParser = fail "FIXME"

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

-- | This corresponds to the hostname, e.g. www.example.org
regNameParser :: Parser ByteString
regNameParser = urlDecode' <$> A.takeWhile1 (inClass validForRegName)
  where
    validForRegName = pctEncoded ++ subDelims ++ unreserved

-- | Only parse a port if the colon signifier is there.
mPortParser :: URIParser (Maybe Port)
mPortParser = word8' colon `thenJust` portParser

-- | Parses port number from the hostname. Colon separator must be
-- handled elsewhere.
portParser :: URIParser Port
portParser = (Port <$> A.takeWhile1 isDigit) `orFailWith` MalformedPort

-- | Path with any number of segments
pathParser :: URIParser ByteString
pathParser = pathParser' A.many'

-- | Path with at least 1 segment
pathParser1 :: URIParser ByteString
pathParser1 = pathParser' A.many1'

-- | Parses the path section of a url. Note that while this can take
-- percent-encoded characters, it does not itself decode them while parsing.
pathParser' :: (Parser ByteString -> Parser [ByteString]) -> URIParser ByteString
pathParser' repeatParser = (mconcat <$> repeatParser segmentParser) `orFailWith` MalformedPath
  where
    segmentParser = mconcat <$> sequence [string "/", A.takeWhile (inClass pchar)]

-- | This parser is being a bit pragmatic. The query section in the
-- spec does not identify the key/value format used in URIs, but that
-- is what most users are expecting to see. One alternative could be
-- to just expose the query string as a string and offer functions on
-- URI to parse a query string to a Query.
queryParser :: URIParser Query
queryParser = do
  mc <- peekWord8 `orFailWith` OtherError "impossible peekWord8 error"
  case mc of
    Just c -> if c == question
              then skip' 1 *> itemsParser
              else fail' MalformedQuery
    _      -> pure mempty
  where
    itemsParser = Query <$> A.sepBy' queryItemParser (word8' ampersand)

-- | When parsing a single query item string like "foo=bar", turns it
-- into a key/value pair as per convention, with the value being
-- optional. & separators need to be handled further up.
queryItemParser :: URIParser (ByteString, ByteString)
queryItemParser = do
  s <- A.takeWhile1 validForQuery `orFailWith` MalformedQuery
  let (k, vWithEquals) = BS.break (== equals) s
  let v = BS.drop 1 vWithEquals
  return (urlDecodeQuery k, urlDecodeQuery v)
  where
    validForQuery = inClass ('?':'/':'[':']':delete '&' pchar)

-- | Only parses a fragment if the # signifiier is there
mFragmentParser :: URIParser (Maybe ByteString)
mFragmentParser = word8' hash `thenJust` fragmentParser
  where
    hash = 35

-- TODO: may want to just take till EOS and then check and see if its valid

-- | The final piece of a uri, e.g. #fragment, minus the #.
fragmentParser :: URIParser ByteString
fragmentParser = A.takeWhile1 validFragmentWord `orFailWith` MalformedFragment
  where
    validFragmentWord = inClass ('?':'/':pchar)

                             ------------------------
                             -- Grammar Components --
                             ------------------------

isAlpha :: Word8 -> Bool
isAlpha = inClass alpha

isDigit :: Word8 -> Bool
isDigit = inClass digit

pchar :: String
pchar = pctEncoded ++ subDelims ++ ":@" ++ unreserved


-- Very important!  When concatenating this to other strings to make larger
-- character classes, you must put this at the end because the '-' character
-- is treated as a range unless it's at the beginning or end.
unreserved :: String
unreserved = alphaNum ++ "~._-"


-- | pc-encoded technically is % HEXDIG HEXDIG but that's handled by
-- the previous alphaNum constraint. May need to double back with a
-- parser to ensure pct-encoded never exceeds 2 hexdigs after
pctEncoded :: String
pctEncoded = "%"

subDelims :: String
subDelims = "!$&'()*+,;="

alphaNum :: String
alphaNum = alpha ++ digit

alpha :: String
alpha = "a-zA-Z"

digit :: String
digit = "0-9"

colon :: Word8
colon = 58

oBracket :: Word8
oBracket = 91

cBracket :: Word8
cBracket = 93

equals :: Word8
equals = 61

question :: Word8
question = 63

ampersand :: Word8
ampersand = 38

          ---------------------------
          -- ByteString Utilitiies --
          ---------------------------

-- FIXME: theres probably a much better way to do this

-- | Convert a bytestring into an int representation. Assumes the
-- entire string is comprised of 0-9 digits.
bsToNum :: ByteString -> Int
bsToNum s = sum $ zipWith (*) (reverse ints) [10 ^ x | x <- [0..] :: [Int]]
  where
    w2i w = fromIntegral $ w - 48
    ints  = map w2i . BS.unpack $ s

-- | Decoding specifically for the query string, which decodes + as
-- space.
urlDecodeQuery :: ByteString -> ByteString
urlDecodeQuery = urlDecode plusToSpace
  where
    plusToSpace = True

-- | Decode any part of the URL besides the query, which decodes + as
-- space.
urlDecode' :: ByteString -> ByteString
urlDecode' = urlDecode plusToSpace
  where
    plusToSpace = False

          ----------------------------------------
          -- Parsing with Strongly-Typed Errors --
          ----------------------------------------
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

-- | Use with caution. Catch a parser failing and return Nothing.
mParse :: Parser' e a -> Parser' e (Maybe a)
mParse p = option Nothing (Just <$> p)

-- | If the first parser succeeds, discard the result and use the
-- second parser (which may fail). If the first parser fails, return
-- Nothing. This is used to check a benign precondition that indicates
-- the presence of a parsible token, i.e. ? preceeding a query.
thenJust :: Parser' e a -> Parser' e b -> Parser' e (Maybe b)
thenJust p1 p2 = p1 *> (Just <$> p2) <|> pure Nothing

-- | Lift a word8 Parser into a strongly error typed parser. This will
-- generate a "stringy" error message if it fails, so you should
-- probably be prepared to exit with a nicer error further up.
word8' :: Word8 -> Parser' e Word8
word8' = Parser' . word8

-- | Skip exactly 1 character. Fails if the character isn't
-- there. Generates a "stringy" error.
skip' :: Int -> Parser' e ()
skip' = Parser' . void . A.take

-- | Lifted version of the string token parser. Same caveats about
-- "stringy" errors apply.
string' :: ByteString -> Parser' e ByteString
string' = Parser' . string

-- | Combinator for tunnelling more specific error types through the
-- attoparsec machinery using read/show.
orFailWith :: (Show e, Read e) => Parser a -> e -> Parser' e a
orFailWith p e = Parser' p <|> fail' e

-- | Should be preferred to fail'
fail' :: (Show e, Read e) => e -> Parser' e a
fail' = fail . show

-- | Stronger-typed variation of parseOnly'. Consumes all input.
parseOnly' :: (Read e, Show e)
              => (String -> e) -- ^ Fallback if we can't parse a failure message for the sake of totality.
              -> Parser' e a
              -> ByteString
              -> Either e a
parseOnly' noParse (Parser' p) = fmapL readWithFallback . parseOnly p
  where
    readWithFallback s = fromMaybe (noParse s) (readMaybe . stripAttoparsecGarbage $ s)

-- | Our pal Control.Monad.fail is how attoparsec propagates
-- errors. If you throw an error string with fail (your only choice),
-- it will *always* prepend it with "Failed reading: ". At least in
-- this version. That may change to something else and break this workaround.
stripAttoparsecGarbage :: String -> String
stripAttoparsecGarbage = stripPrefix' "Failed reading: "

-- | stripPrefix where it is a noop if the prefix doesn't exist.
stripPrefix' :: Eq a => [a] -> [a] -> [a]
stripPrefix' pfx s = fromMaybe s $ stripPrefix pfx s

fmapL :: (a -> b) -> Either a r -> Either b r
fmapL f = either (Left . f) Right
