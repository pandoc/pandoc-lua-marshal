{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|
Module      : Main
Copyright   : © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Tests for the pandoc types handling in Lua.
-}
module Main (main) where

import Control.Monad (void)
import HsLua as Lua
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Lua (translateResultsFromFile)

main :: IO ()
main = do
  luaTest <- run @Lua.Exception $ do
    openlibs
    translateResultsFromFile "test/test-pandoc-types.lua"
  defaultMain $ testGroup "hslua-pandoc-types" [tests, luaTest]

-- | Basic tests
tests :: TestTree
tests = testGroup "Basic tests"
  [ testCase "Sample test" $
      pure ()
  ]
