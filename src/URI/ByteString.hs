{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module URI.ByteString where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Monoid
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString       as A
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.Word
-------------------------------------------------------------------------------


------------------------------------------------------------------------------

-- | Required first component to referring to a specification for the
-- remainder of the URI's components
newtype Scheme = Scheme ByteString deriving (Show, Eq)
newtype Host = Host ByteString deriving (Show, Eq)
--TODO: probably a numeric type
newtype Port = Port ByteString deriving (Show, Eq)

data Authority = Authority
   { userInfo :: Maybe UserInfo
   , host :: Host
   , port :: Maybe Port -- probably a numeric type
   }  deriving (Show, Eq) --TODO


data UserInfo = UserInfo
  { username :: ByteString
  , password :: ByteString
  } deriving (Show, Eq)

data Query = Query deriving (Show, Eq)

data URI = URI
    { uriScheme    :: Scheme
    , uriAuthority :: Maybe Authority
    , uriPath      :: ByteString
    , uriQuery     :: Query
    , uriFragment  :: Maybe ByteString
    } deriving (Show, Eq)

                             --------------------
                             -- URI Parser --
                             --------------------

--TODO: quickcheck Show, Read
--TODO:
data SchemaError = NonAlphaLeading
                 | InvalidChars
                 | MissingColon deriving (Show, Eq, Read)

data URIParseError = MalformedSchema SchemaError
                   | MalformedUserInfo
                   | MalformedQuery
                   | MalformedFragment
                   | MalformedHost
                   | MalformedPort
                   | MalformedPath
                   | IncompleteInput
                   | OtherError String deriving (Show, Eq, Read) --TODO; othererror fallback

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
heirPartParser = authWithPathParser
  where
    authWithPathParser = string' "//" *> ((,) <$> mAuthorityParser <*> pathParser)

mAuthorityParser :: URIParser (Maybe Authority)
mAuthorityParser = mParse authorityParser

userInfoParser :: URIParser UserInfo
userInfoParser =  (uiTokenParser <* word8 atSym) `orFailWith` MalformedUserInfo
  where
    atSym = 64
    uiTokenParser = do
      ui <- A.takeWhile1 validForUserInfo
      let (user, passWithColon) = BS.break (== colon) ui
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
regNameParser = A.takeWhile1 $ inClass validForRegName
  where
    validForRegName = unreserved ++ pctEncoded ++ subDelims

mPortParser :: URIParser (Maybe Port)
mPortParser = word8' colon `thenJust` portParser

portParser :: URIParser Port
portParser = (Port <$> A.takeWhile1 isDigit) `orFailWith` MalformedPort

--TODO: this needs work, need to basically take till the next token
pathParser :: URIParser ByteString
pathParser = (mconcat <$> A.many1' segmentParser) `orFailWith` MalformedPath
  where
    segmentParser = mconcat <$> sequence [string "/", A.takeWhile (inClass pchar)]

--TODO
queryParser :: URIParser Query
queryParser = pure Query

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
          ---------------------------
          -- ByteString Utilitiies --
          ---------------------------

-- FIXME: theres probably a much better way to do this
bsToNum :: ByteString -> Int
bsToNum s = sum $ zipWith (*) (reverse ints) [10 ^ x | x <- [0..] :: [Int]]
  where
    w2i w = fromIntegral $ w - 48
    ints  = map w2i . BS.unpack $ s


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
