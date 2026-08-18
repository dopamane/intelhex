{-# LANGUAGE OverloadedStrings #-}

module Codec.IntelHEX (readIntelHEX, buildIntelHEX, writeIntelHEX) where

import Data.Bits
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.String
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

writeIntelHEX :: ByteString -> ByteString
writeIntelHEX = toLazyByteString . buildIntelHEX

buildIntelHEX :: ByteString -> Builder
buildIntelHEX = w 0

w :: Word16 -> ByteString -> Builder
w addr bs
  | BS.null bs = ":00000001FF"
  | otherwise  = mconcat
    [ ":", unbyte bc, unbyte addrh, unbyte addrl, "00", foldMap unbyte d
    , unbyte $ complement (sum (bc:addrh:addrl:d)) + 1, "\n"
    , w (addr + fromIntegral bc) rest
    ]
  where
    (h, rest) = BS.splitAt 16 bs
    bc = fromIntegral $ BS.length h
    addrh = fromIntegral $ addr `shiftR` 8
    addrl = fromIntegral addr
    d = BS.unpack h

unbyte :: Word8 -> Builder
unbyte b = fromString $ toUpper <$> showHex (b `shiftR` 4) "" ++ showHex (b .&. 0xF) ""
