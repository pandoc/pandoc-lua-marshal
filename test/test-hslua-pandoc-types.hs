{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module      : Main
Copyright   : © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Tests for the pandoc types handling in Lua.
-}
module Main (main) where

import Control.Monad (forM_)
import Data.Data (Data, dataTypeConstrs, dataTypeOf, showConstr)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import HsLua as Lua
import HsLua.Pandoc.Types
import Text.Pandoc.Definition (ListNumberStyle, ListNumberDelim)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Lua (translateResultsFromFile)

main :: IO ()
main = do
  listAttributeTests <- run @Lua.Exception $ do
    openlibs
    register' mkListAttributes
    forM_ (constructors (Proxy @ListNumberStyle)) $ \c -> do
      pushString c
      setglobal (fromString c)
    forM_ (constructors (Proxy @ListNumberDelim)) $ \c -> do
      pushString c
      setglobal (fromString c)
    translateResultsFromFile "test/test-listattributes.lua"
  listTest <- run @Lua.Exception $ do
    openlibs
    pushListModule
    setglobal "List"
    translateResultsFromFile "test/test-list-module.lua"
  luaTest <- run @Lua.Exception $ do
    openlibs
    translateResultsFromFile "test/test-pandoc-types.lua"
  defaultMain $ testGroup "hslua-pandoc-types"
    [ tests
    , listAttributeTests
    , listTest
    , luaTest
    ]

-- | Basic tests
tests :: TestTree
tests = testGroup "Basic tests"
  [ testCase "Sample test" $
      pure ()
  ]

register' :: LuaError e => DocumentedFunction e -> LuaE e ()
register' f = do
  pushDocumentedFunction f
  setglobal (functionName f)

constructors :: forall a. Data a => Proxy a -> [String]
constructors _ = map showConstr . dataTypeConstrs . dataTypeOf @a $ undefined
