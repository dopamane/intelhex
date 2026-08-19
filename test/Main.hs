{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.IntelHEX
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.String
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain $ testGroup "Test.IntelHEX"
  [ helloWorldTest
  , readAfterWriteTest
  ]

helloWorldTest :: TestTree
helloWorldTest = testCaseSteps "hello-world" $ \step -> do
  step "read"
  readIntelHEX helloWorldHEX @?= "Hello, World\n"
  step "write"
  writeIntelHEX "Hello, World\n" @?= fromString (init helloWorldHEX)

helloWorldHEX :: String
helloWorldHEX = ":0D00000048656C6C6F2C20576F726C640AA1\n:00000001FF\n"

readAfterWriteTest :: TestTree
readAfterWriteTest = testProperty "read-after-write" $ property $ do
  bs <- forAll $ BS.fromStrict <$> Gen.bytes (Range.linear 0 16384)
  (readIntelHEX . BSC.unpack . writeIntelHEX) bs === bs
