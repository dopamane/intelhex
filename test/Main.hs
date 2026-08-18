{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.IntelHEX
import Data.String
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Test.IntelHEX"
  [ helloWorldTest
  ]

helloWorldTest :: TestTree
helloWorldTest = testCaseSteps "hello-world" $ \step -> do
  step "read"
  readIntelHEX helloWorldHEX @?= "Hello, World\n"
  step "write"
  writeIntelHEX "Hello, World\n" @?= fromString (init helloWorldHEX)

helloWorldHEX :: String
helloWorldHEX = ":0D00000048656C6C6F2C20576F726C640AA1\n:00000001FF\n"
