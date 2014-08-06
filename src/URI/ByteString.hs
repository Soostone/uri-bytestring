{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}

module URI.ByteString where

-------------------------------------------------------------------------------
import           Control.Error
import           Data.Aeson
import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Regex.PCRE.Light
-------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Matches the rough structure of URIs.  Should be improved.
--
-- See the following link for a nice overview:
-- http://medialize.github.io/URI.js/about-uris.html
data URI = URI
    { uriScheme    :: ByteString
    , uriAuthority :: ByteString
    , uriPath      :: ByteString
    , uriQuery     :: ByteString
    , uriFragment  :: ByteString
    } deriving (Show,Eq)


                             --------------------
                             -- URI Parser --
                             --------------------


parseURI :: ByteString -> Either Text URI
parseURI bs = do
    res <- note "URI pattern match failed" $ match regexA bs []
    case res of
      [_,scheme,authority,path,qs] -> Right $ URI scheme authority path qs ""
      _ -> Left "URI pattern matched incorrect structure"
  where
    regexA = compile "^([^:]*)://([^/]*)(/[^?]*)\\?(.*)$" []


