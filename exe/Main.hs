module Main (main) where

import Codec.IntelHEX
import qualified Data.ByteString.Lazy as BS
import Data.Version
import Options.Applicative
import Paths_intelhex

main :: IO ()
main = do
  cli <- runCLI $ showVersion version
  case cli of
    ReadHEX -> BS.putStr . readIntelHEX =<< getContents
    WriteHEX -> fail "not implemented"

data CLI = ReadHEX | WriteHEX

runCLI :: String -> IO CLI
runCLI = customExecParser prefs' . pinfo
  where
    prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $ progDesc "Intel HEX"
  where
    parser =
      flag' ReadHEX (short 'r' <> long "read" <> help "Read Intel HEX and output binary")
        <|> flag' WriteHEX (short 'w' <> long "write" <> help "Read binary and output Intel HEX")
