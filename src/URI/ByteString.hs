{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}

module URI.ByteString where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.DeepSeq.Generics
import           Control.Error
import           Control.Monad
import           GHC.Generics
import           GHC.Read
import           Data.List (delete)
import           Data.Monoid
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word
import           Network.HTTP.Types.URI (urlDecode)
import           Text.Read.Lex
-------------------------------------------------------------------------------


------------------------------------------------------------------------------

-- | Required first component to referring to a specification for the
-- remainder of the URI's components
newtype Scheme = Scheme ByteString deriving (Show, Eq, NFData)
newtype Host = Host ByteString deriving (Show, Eq, NFData)

-- | While some libraries have chosen to limit this to a Word16, the
-- spec seems to only specify that the string be comprised of digits.
newtype Port = Port ByteString deriving (Show, Eq, NFData)

data Authority = Authority
   { userInfo :: Maybe UserInfo
   , host :: Host
   , port :: Maybe Port -- probably a numeric type
   }  deriving (Show, Eq, Generic)

instance NFData Authority

data UserInfo = UserInfo
  { username :: ByteString
  , password :: ByteString
  } deriving (Show, Eq, Generic)

instance NFData UserInfo

newtype Query = Query [(ByteString, Maybe ByteString)]
              deriving (Show, Eq, Monoid)

data URI = URI
    { uriScheme    :: Scheme
    , uriAuthority :: Maybe Authority
    , uriPath      :: ByteString
    , uriQuery     :: Query
    , uriFragment  :: Maybe ByteString
    } deriving (Show, Eq, Generic)

instance NFData URI

                             --------------------
                             -- URI Parser --
                             --------------------

--TODO: quickcheck Show, Read
--TODO:
data SchemaError = NonAlphaLeading
                 | InvalidChars
                 | MissingColon deriving (Show, Eq, Read, Generic)

instance NFData SchemaError

data URIParseError = MalformedSchema SchemaError
                   | MalformedUserInfo
                   | MalformedQuery
                   | MalformedFragment
                   | MalformedHost
                   | MalformedPort
                   | MalformedPath
                   | IncompleteInput
                   | OtherError String deriving (Show, Eq, Generic)

instance Read URIParseError where
  readPrec = parens $ do
    tok <- lexP
    case tok of
      String s -> return $ OtherError s
      Ident s  -> parseFallback s
      _        -> fail "no parse"
    where
      parseFallback "MalformedSchema"   = MalformedSchema <$> readPrec
      parseFallback "MalformedUserInfo" = return MalformedUserInfo
      parseFallback "MalformedQuery"    = return MalformedQuery
      parseFallback "MalformedFragment" = return MalformedFragment
      parseFallback "MalformedHost"     = return MalformedHost
      parseFallback "MalformedPort"     = return MalformedPort
      parseFallback "MalformedPath"     = return MalformedPath
      parseFallback "IncompleteInput"   = return IncompleteInput
      parseFallback "OtherError"        = OtherError <$> readPrec
      parseFallback _                   = fail "no parse"

instance NFData URIParseError

parseUri :: ByteString -> Either URIParseError URI
parseUri = parseOnly' uriParser

type URIParser = Parser' URIParseError

uriParser :: URIParser URI
uriParser = do
  scheme <- schemeParser
  void $ word8 colon `orFailWith` MalformedSchema MissingColon

  (authority, path) <- heirPartParser
  query <- queryParser
  frag  <- mFragmentParser
  return $ URI scheme authority path query frag

schemeParser :: URIParser Scheme
schemeParser = do
  c    <- satisfy isAlpha           `orFailWith` MalformedSchema NonAlphaLeading
  rest <- A.takeWhile isSchemeValid `orFailWith` MalformedSchema InvalidChars
  return $ Scheme $ c `BS.cons` rest
  where
    isSchemeValid = inClass $ "-+." ++ alphaNum

--TODO: handle absolute, noscheme, empty
heirPartParser :: URIParser (Maybe Authority, ByteString)
heirPartParser = authWithPathParser <|>
                 pathAbsoluteParser <|>
                 pathRootlessParser <|>
                 pathEmptyParser

authWithPathParser :: URIParser (Maybe Authority, ByteString)
authWithPathParser = string' "//" *> ((,) <$> mAuthorityParser <*> pathParser)

pathAbsoluteParser :: URIParser (Maybe Authority, ByteString)
pathAbsoluteParser = string' "/" *> pathRootlessParser

pathRootlessParser :: URIParser (Maybe Authority, ByteString)
pathRootlessParser = (,) <$> pure Nothing <*> pathParser1

pathEmptyParser :: URIParser (Maybe Authority, ByteString)
pathEmptyParser = undefined

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
    validForUserInfo = inClass $ unreserved ++ pctEncoded ++ subDelims ++ ":"

authorityParser :: URIParser Authority
authorityParser = Authority <$> mParse userInfoParser <*> hostParser <*> mPortParser

--FIXME: without tld validation, invalid ip addresses pass through
hostParser :: URIParser Host
hostParser = (Host <$> parsers) `orFailWith` MalformedHost
  where
    parsers = ipLiteralParser <|> ipV4Parser <|> regNameParser
    ipLiteralParser = word8 oBracket *> (ipV6Parser <|> ipVFutureParser) <* word8 cBracket

ipV6Parser :: Parser ByteString
ipV6Parser = fail "FIXME"

ipVFutureParser :: Parser ByteString
ipVFutureParser = fail "FIXME"

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

regNameParser :: Parser ByteString
regNameParser = urlDecode' <$> A.takeWhile1 (inClass validForRegName)
  where
    validForRegName = unreserved ++ pctEncoded ++ subDelims

mPortParser :: URIParser (Maybe Port)
mPortParser = word8' colon `thenJust` portParser

portParser :: URIParser Port
portParser = (Port <$> A.takeWhile1 isDigit) `orFailWith` MalformedPort

pathParser :: URIParser ByteString
pathParser = pathParser' A.many'

pathParser1 :: URIParser ByteString
pathParser1 = pathParser' A.many1'

-- | Parses the path section of a url. Note that while this can take
-- percent-encoded characters, it does not itself decode them while parsing.
pathParser' :: (Parser ByteString -> Parser [ByteString]) -> URIParser ByteString
pathParser' repeatParser = (mconcat <$> repeatParser segmentParser) `orFailWith` MalformedPath
  where
    segmentParser = mconcat <$> sequence [string "/", A.takeWhile (inClass pchar)]

-- | This parser is being a bit pragmatic. The query section in the spec does not identify the key/value format used in URIs
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

queryItemParser :: URIParser (ByteString, Maybe ByteString)
queryItemParser = do
  s <- A.takeWhile1 validForQuery `orFailWith` MalformedQuery
  let (k, vWithEquals) = BS.break (== equals) s
  let v = case BS.drop 1 vWithEquals of
             "" -> Nothing
             v' -> Just v'
  return (urlDecodeQuery k, urlDecodeQuery <$> v)
  where
    validForQuery = inClass ('?':'/':delete '&' pchar)

mFragmentParser :: URIParser (Maybe ByteString)
mFragmentParser = word8' hash `thenJust` fragmentParser
  where
    hash = 35

-- TODO: may want to just take till EOS and then check and see if its valid
fragmentParser :: URIParser ByteString
fragmentParser = A.takeWhile validFragmentWord `orFailWith` MalformedFragment
  where
    validFragmentWord = inClass ('?':'/':pchar)

                             ------------------------
                             -- Grammar Components --
                             ------------------------

--TODO: use characters rather than inClass for performance
isAlpha :: Word8 -> Bool
isAlpha = inClass alpha

isDigit :: Word8 -> Bool
isDigit = inClass digit

pchar :: String
pchar = unreserved ++ pctEncoded ++ subDelims ++ ":@"

unreserved :: String
unreserved = "-._~" ++ alphaNum


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
bsToNum :: ByteString -> Int
bsToNum s = sum $ zipWith (*) (reverse ints) [10 ^ x | x <- [0..] :: [Int]]
  where
    w2i w = fromIntegral $ w - 48
    ints  = map w2i . BS.unpack $ s

urlDecodeQuery :: ByteString -> ByteString
urlDecodeQuery = urlDecode plusToSpace
  where
    plusToSpace = True

urlDecode' :: ByteString -> ByteString
urlDecode' = urlDecode plusToSpace
  where
    plusToSpace = False

          ----------------------------------------
          -- Parsing with Strongly-Typed Errors --
          ----------------------------------------
newtype Parser' e a = Parser' (Parser a)
                    deriving ( Functor
                             , Applicative
                             , Alternative
                             , Monad
                             , MonadPlus
                             , Monoid)

mParse :: Parser' e a -> Parser' e (Maybe a)
mParse p = option Nothing (Just <$> p)

thenJust :: Parser' e a -> Parser' e b -> Parser' e (Maybe b)
thenJust p1 p2 = p1 *> (Just <$> p2) <|> pure Nothing

word8' :: Word8 -> Parser' e Word8
word8' = Parser' . word8

skip' :: Int -> Parser' e ()
skip' = Parser' . void . A.take


string' :: ByteString -> Parser' e ByteString
string' = Parser' . string

orFailWith :: (Show e, Read e) => Parser a -> e -> Parser' e a
orFailWith p e = Parser' p <|> fail' e

fail' :: (Show e, Read e) => e -> Parser' e a
fail' = fail . show

type Result' e = IResult' e ByteString

data IResult' e i r = Fail' i [String] e
                    | Partial' (i -> IResult' e i r)
                    | Done' i r

parse' :: (Read e, Show e) => Parser' e a -> ByteString -> Result' e a
parse' (Parser' p) = convertIResult . parse p

parseOnly' :: (Read e, Show e) => Parser' e a -> ByteString -> Either e a
parseOnly' (Parser' p) = fmapL read . parseOnly p

eitherResult' :: e -> IResult' e i r -> Either e r
eitherResult' _ (Done' _ r)   = Right r
eitherResult' _ (Fail' _ _ e) = Left e
eitherResult' incomplete _    = Left incomplete

convertIResult :: (Read  e, Show e) => IResult i r -> IResult' e i r
convertIResult (Fail leftover ctx e) = Fail' leftover ctx $ read e
convertIResult (Partial f)           = Partial' (convertIResult . f)
convertIResult (Done leftover r)     = Done' leftover r
