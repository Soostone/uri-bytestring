{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE CPP                #-}

module URI.ByteString.QQ
    ( uri
    , relativeRef
    ) where

import Language.Haskell.TH.Quote
import URI.ByteString
import Data.ByteString.Char8
import Instances.TH.Lift()

-- | Allows uri literals via QuasiQuotes language extension.
--
-- >>> {-# LANGUAGE QuasiQuotes #-}
-- >>> stackage :: URI
-- >>> stackage = [uri|http://stackage.org|]
uri :: QuasiQuoter
uri = QuasiQuoter { quoteExp = \s ->
                      let
                        parsedURI = either (\err -> error $ show err) id (parseURI laxURIParserOptions (pack s))
                      in
                        [| parsedURI |],
                   quotePat = error "Not implemented.",
                   quoteType = error "Not implemented.",
                   quoteDec = error "Not implemented."
                  }


-------------------------------------------------------------------------------
-- | Allows relative ref literals via QuasiQuotes language extension.
--
-- >>> {-# LANGUAGE QuasiQuotes #-}
-- >>> ref :: RelativeRef
-- >>> ref = [relativeRef|/foo?bar=baz#quux|]
relativeRef :: QuasiQuoter
relativeRef = QuasiQuoter { quoteExp = \s ->
                      let
                        parsedURI = either (\err -> error $ show err) id (parseRelativeRef laxURIParserOptions (pack s))
                      in
                        [| parsedURI |],
                   quotePat = error "Not implemented.",
                   quoteType = error "Not implemented.",
                   quoteDec = error "Not implemented."
                  }
