module URI.Common where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Bits
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Maybe
import           GHC.Word
-------------------------------------------------------------------------------


-- | If the first parser succeeds, discard the result and use the
-- second parser (which may fail). If the first parser fails, return
-- Nothing. This is used to check a benign precondition that indicates
-- the presence of a parsible token, i.e. ? preceeding a query.
thenJust :: (Alternative m) => m a -> m b -> m (Maybe b)
thenJust p1 p2 = p1 *> (Just <$> p2) <|> pure Nothing



-------------------------------------------------------------------------------
-- | Bytestring Utilities
-------------------------------------------------------------------------------


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


-- | This function was extracte from the @http-types@ package. The
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
                Just $ (combine x' y', ys)
            Just (w, ws) -> Just (w, ws)
    hexVal w
        | 48 <= w && w <= 57  = Just $ w - 48 -- 0 - 9
        | 65 <= w && w <= 70  = Just $ w - 55 -- A - F
        | 97 <= w && w <= 102 = Just $ w - 87 -- a - f
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b
