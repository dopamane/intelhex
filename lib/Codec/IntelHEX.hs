module Codec.IntelHEX (readIntelHEX) where

import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Word

readIntelHEX :: String -> ByteString
readIntelHEX = f

f :: String -> ByteString
f (':':'0':'0':'0':'0':'0':'0':'0':'1':_:_:_) = mempty
f (':':b:c:_a3:_a2:_a1:_a0:'0':'0':rest) =
  let (d, rest') = splitAt (fromIntegral (byte b c) * 2) rest
  in readData d <> f (drop 2 rest')
f (' ':rest) = f rest
f ('\n':rest) = f rest
f (c:_) = error $ "unxpected char \'" <> [c] <> "\'"
f [] = error "unexpected eof"

readData :: String -> ByteString
readData "" = mempty
readData (h:l:rest) = BS.singleton (byte h l) <> readData rest
readData _ = error "read data malformed"

byte :: Char -> Char -> Word8
byte h l = nyb h `shiftL` 4 .|. nyb l
  where
    nyb c | isDigit c = n - 48
          | otherwise = n - 87
      where
        n = fromIntegral $ ord $ toLower c
