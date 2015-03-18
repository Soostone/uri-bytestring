{-# LANGUAGE TupleSections #-}
module URI.Parser
    ( uriParser
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as BS
import           Data.Maybe
import           Data.Monoid
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
-------------------------------------------------------------------------------
import           URI.Common
import           URI.Types
-------------------------------------------------------------------------------


--TODO: annotate

uriParser
    :: ( TokenParsing m
       , MonadPlus m)
    => URIParserOptions
    -> m URI
uriParser opts = do
    scheme <- schemeParser
    void $ char ':'

    (authority, path) <- hierPartParser
    query <- queryParser opts
    frag  <- mFragmentParser
    eof
    return $ URI scheme authority path query frag


-------------------------------------------------------------------------------
schemeParser
    :: ( CharParsing m
       , Alternative m
       , Monad m)
    => m Scheme
schemeParser = do
    c    <- letter
    rest <- many schemeValid
    return $ Scheme $ BS.pack (c:rest)
  where
    schemeValid = alphaNum <|> oneOf "-+."


-------------------------------------------------------------------------------
hierPartParser
    :: ( TokenParsing m
       , Alternative m
       , MonadPlus m)
    => m (Maybe Authority, ByteString)
hierPartParser = authWithPathParser <|>
                 pathAbsoluteParser <|>
                 pathRootlessParser <|>
                 pathEmptyParser


-------------------------------------------------------------------------------
authWithPathParser
    :: ( TokenParsing m
       , Alternative m
       , MonadPlus m)
    => m (Maybe Authority, ByteString)
authWithPathParser = string "//" *> ((,) <$> mAuthorityParser <*> pathParser)


-------------------------------------------------------------------------------
mAuthorityParser
    :: ( TokenParsing m
       , Applicative m
       , MonadPlus m)
    => m (Maybe Authority)
mAuthorityParser = optional authorityParser


-------------------------------------------------------------------------------
authorityParser
    :: ( TokenParsing m
       , Applicative m
       , MonadPlus m)
    => m Authority
authorityParser = Authority <$> optional userInfoParser <*> hostParser <*> mPortParser


-------------------------------------------------------------------------------
userInfoParser
    :: ( CharParsing m
       , Applicative m
       , Monad m)
    => m UserInfo
userInfoParser =  (uiTokenParser <* char '@')
  where
    uiTokenParser = do
      ui <- BS.pack <$> many validForUserInfo
      let (user, passWithColon) = BS.break (== ':') $ urlDecode' ui
      let pass = BS.drop 1 passWithColon
      return $ UserInfo user pass
    validForUserInfo = pctEncoded <|> subDelims <|> char ':' <|> unreserved


-------------------------------------------------------------------------------
hostParser
    :: ( TokenParsing m
       , Alternative m
       , MonadPlus m )
    => m Host
hostParser = (Host <$> parsers)
  where
    parsers = ipLiteralParser <|> ipV4Parser <|> regNameParser
    ipLiteralParser = brackets $ ipV6Parser <|> ipVFutureParser


-------------------------------------------------------------------------------
ipV6Parser :: Monad m => m ByteString
ipV6Parser = fail "FIXME"


-------------------------------------------------------------------------------
ipVFutureParser :: Monad m => m ByteString
ipVFutureParser = fail "FIXME"


-------------------------------------------------------------------------------
ipV4Parser
    :: ( CharParsing m
       , Alternative m
       , MonadPlus m)
    => m ByteString
ipV4Parser = mconcat <$> sequence [ decOctet
                                  , dot
                                  , decOctet
                                  , dot
                                  , decOctet
                                  , dot
                                  , decOctet]
  where
    decOctet = do
      s <- BS.pack <$> some digit
      let len = BS.length s
      guard $ len > 0 && len <= 3
      let num = bsToNum s
      guard $ num >= 1 && num <= 255
      return s
    dot = BS.singleton <$> char '.'

-------------------------------------------------------------------------------
regNameParser :: (Alternative m, CharParsing m) => m ByteString
regNameParser = urlDecode' . BS.pack <$> some validForRegName
  where
    validForRegName = pctEncoded <|> subDelims <|> unreserved


-------------------------------------------------------------------------------
pathParser :: (CharParsing m, Monad m) => m ByteString
pathParser = pathParser' many

-------------------------------------------------------------------------------
-- | Path with at least 1 segment
pathParser1 :: (CharParsing m, Monad m) => m ByteString
pathParser1 = pathParser' some


-------------------------------------------------------------------------------
pathParser'
    :: (CharParsing m
       , Monad m)
    => (m ByteString -> m [ByteString])
    -> m ByteString
pathParser' repeatParser = (mconcat <$> repeatParser segmentParser)
  where
    segmentParser = BS.pack . mconcat <$> sequence [string "/", many pchar]


-------------------------------------------------------------------------------
pathAbsoluteParser
    :: ( CharParsing m
       , Monad m)
    => m (Maybe Authority, ByteString)
pathAbsoluteParser = char '/' *> pathRootlessParser


-------------------------------------------------------------------------------
pathRootlessParser
    :: ( CharParsing m
       , Monad m)
    => m (Maybe Authority, ByteString)
pathRootlessParser = (,) <$> return Nothing <*> pathParser1


-------------------------------------------------------------------------------
pathEmptyParser
    :: ( CharParsing m
       , Monad m)
    => m (Maybe Authority, ByteString)
pathEmptyParser = (try pchar *> fail "path not empty") <|> --FIXME: this is totally wrong, will always hit the right case
                  (return emptyCase)
  where
    emptyCase = (Nothing, mempty)



-------------------------------------------------------------------------------
queryParser opts = (char '?' *> itemsParser) <|>
                   (try (char '#') *> pure mempty) <|>
                   (eof *> pure mempty) <|>
                   fail "MalformedPath"
  where
    itemsParser = Query <$> sepBy (queryItemParser opts) (char '&')


-------------------------------------------------------------------------------
queryItemParser opts = do
  -- s <- BS.pack <$> some (satisfy $ upoValidQueryChar opts) --TODO
  s <- BS.pack <$> some (oneOf "?/" <|> except '&' pchar)
  let (k, vWithEquals) = BS.break (== '=') s
  let v = BS.drop 1 vWithEquals
  return (urlDecodeQuery k, urlDecodeQuery v)


-------------------------------------------------------------------------------
mFragmentParser :: (CharParsing m) => m (Maybe ByteString)
mFragmentParser = char '#' `thenJust` fragmentParser


-------------------------------------------------------------------------------
fragmentParser :: (CharParsing m) => m ByteString
fragmentParser = BS.pack <$> some validFragmentWord
  where
    validFragmentWord = pchar <|> oneOf "?/"


-------------------------------------------------------------------------------
mPortParser
    :: (CharParsing m)
    => m (Maybe Port)
mPortParser = char ':' `thenJust` portParser


-------------------------------------------------------------------------------
portParser :: (CharParsing m) => m Port
portParser = Port . BS.pack <$> some digit



-------------------------------------------------------------------------------
orFailWith :: Alternative m => m a -> e -> EitherT e m a
orFailWith f e = EitherT $ (Right <$> f) <|> (pure $ Left e)


-------------------------------------------------------------------------------
pctEncoded :: CharParsing m => m Char
pctEncoded = char '%'


subDelims :: CharParsing m => m Char
subDelims = oneOf "!$&'()*+,;="


unreserved :: CharParsing m => m Char
unreserved = alphaNum <|> oneOf "~._-"


pchar :: CharParsing m => m Char
pchar = pctEncoded <|> subDelims <|> oneOf ":@" <|> unreserved


skipOne :: (Functor f, CharParsing f) => f ()
skipOne = void anyChar


except
    :: ( CharParsing m
       , Alternative m
       , MonadPlus m)
    => Char
    -> m Char
    -> m Char
except c f = (char c *> mzero) <|> f
