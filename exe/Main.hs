module Main (main) where

import Codec.IntelHEX
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = BS.putStr . readIntelHEX =<< getContents
