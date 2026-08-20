module Main (main) where

import Codec.IntelHEX
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BS
import Data.Version
import Options.Applicative
import Paths_intelhex
import System.IO

main :: IO ()
main = do
  cli <- runCLI $ showVersion version
  dat <- case cli of
    ReadHEX -> readIntelHEX <$> getContents
    WriteHEX -> writeIntelHEX <$> BS.getContents
  hPutBuilder stdout dat

data CLI = ReadHEX | WriteHEX

runCLI :: String -> IO CLI
runCLI = customExecParser prefs' . pinfo
  where
    prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $ progDesc "Intel HEX"
  where
    parser = flag' ReadHEX  (short 'r' <> long "read"  <> help "Read Intel HEX and output binary")
         <|> flag' WriteHEX (short 'w' <> long "write" <> help "Input binary and write Intel HEX")
