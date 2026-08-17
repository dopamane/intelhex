module Codec.IntelHEX (readIntelHEX, writeIntelHEX) where

import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Word
import Numeric

readIntelHEX :: String -> ByteString
readIntelHEX = r

r :: String -> ByteString
r (':':'0':'0':'0':'0':'0':'0':'0':'1':_:_:_) = mempty
r (':':b:c:_a3:_a2:_a1:_a0:'0':'0':rest) =
  let (d, rest') = splitAt (fromIntegral (byte b c) * 2) rest
  in readData d <> r (drop 2 rest')
r (' ':rest) = r rest
r ('\n':rest) = r rest
r (c:_) = error $ "unxpected char \'" <> [c] <> "\'"
r [] = error "unexpected eof"

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

writeIntelHEX :: ByteString -> String
writeIntelHEX = w 0

w :: Word16 -> ByteString -> String
w addr bs
  | BS.null bs = ":00000001FF"
  | otherwise  =
    let bc = fromIntegral $ BS.length h
        addrStr = unbyte (fromIntegral $ addr `shiftR` 8) ++ unbyte (fromIntegral addr)
        d = foldMap unbyte $ BS.unpack h
        cs = "FF"
    in ":" ++ unbyte bc ++ addrStr ++ "00" ++ d ++ cs ++ "\n" ++ w (addr + fromIntegral bc) rest
  where
    (h, rest) = BS.splitAt 16 bs

unbyte :: Word8 -> String
unbyte b = toUpper <$> showHex (b `shiftR` 4) "" ++ showHex (b .&. 0xF) ""
