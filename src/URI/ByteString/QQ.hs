{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module URI.ByteString.QQ
  ( uri,
    relativeRef,
  )
where

import Data.ByteString.Char8
import Instances.TH.Lift ()
import Language.Haskell.TH.Quote
import URI.ByteString

-- $setup
-- >>> :set -XQuasiQuotes

-- | Allows uri literals via QuasiQuotes language extension.
--
-- >>> stackage = [uri|http://stackage.org|]
uri :: QuasiQuoter
uri =
  QuasiQuoter
    { quoteExp = \s ->
        let parsedURI = either (\err -> error $ show err) id (parseURI laxURIParserOptions (pack s))
         in [|parsedURI|],
      quotePat = error "Not implemented.",
      quoteType = error "Not implemented.",
      quoteDec = error "Not implemented."
    }

-------------------------------------------------------------------------------

-- | Allows relative ref literals via QuasiQuotes language extension.
--
-- >>> ref = [relativeRef|/foo?bar=baz#quux|]
relativeRef :: QuasiQuoter
relativeRef =
  QuasiQuoter
    { quoteExp = \s ->
        let parsedURI = either (\err -> error $ show err) id (parseRelativeRef laxURIParserOptions (pack s))
         in [|parsedURI|],
      quotePat = error "Not implemented.",
      quoteType = error "Not implemented.",
      quoteDec = error "Not implemented."
    }
