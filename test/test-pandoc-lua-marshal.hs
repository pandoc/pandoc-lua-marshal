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
import Text.Pandoc.Lua.Marshal
import Text.Pandoc.Definition
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Lua (translateResultsFromFile)

main :: IO ()
main = do
  listTests <- run @Lua.Exception $ do
    openlibs
    pushListModule *> setglobal "List"
    translateResultsFromFile "test/test-list.lua"

  listAttributeTests <- run @Lua.Exception $ do
    openlibs
    register' mkListAttributes
    registerConstants (Proxy @ListNumberStyle)
    registerConstants (Proxy @ListNumberDelim)
    translateResultsFromFile "test/test-listattributes.lua"

  attrTests <- run @Lua.Exception $ do
    openlibs
    pushListModule *> setglobal "List"
    register' mkAttr
    register' mkAttributeList
    translateResultsFromFile "test/test-attr.lua"

  citationTests <- run @Lua.Exception $ do
    openlibs
    pushListModule *> setglobal "List"
    register' mkCitation
    registerConstants (Proxy @CitationMode)
    forM_ inlineConstructors register'
    translateResultsFromFile "test/test-citation.lua"

  inlineTests <- run @Lua.Exception $ do
    openlibs
    pushListModule *> setglobal "List"
    register' mkAttr
    register' mkCitation
    registerConstants (Proxy @CitationMode)
    registerConstants (Proxy @MathType)
    registerConstants (Proxy @QuoteType)
    forM_ inlineConstructors register'
    translateResultsFromFile "test/test-inline.lua"

  blockTests <- run @Lua.Exception $ do
    openlibs
    pushListModule *> setglobal "List"
    register' mkAttr
    register' mkListAttributes
    registerConstants (Proxy @Alignment)
    registerConstants (Proxy @ListNumberStyle)
    registerConstants (Proxy @ListNumberStyle)
    forM_ inlineConstructors register'
    forM_ blockConstructors register'
    translateResultsFromFile "test/test-block.lua"

  metavalueTests <- run @Lua.Exception $ do
    openlibs
    pushListModule *> setglobal "List"
    forM_ metaValueConstructors register'
    translateResultsFromFile "test/test-metavalue.lua"

  pandocTests <- run @Lua.Exception $ do
    openlibs
    pushListModule *> setglobal "List"
    register' mkMeta
    register' mkPandoc
    forM_ inlineConstructors register'
    forM_ blockConstructors register'
    translateResultsFromFile "test/test-pandoc.lua"

  defaultMain $ testGroup "pandoc-lua-marshal"
    [ tests
    , listTests
    , listAttributeTests
    , attrTests
    , citationTests
    , inlineTests
    , blockTests
    , metavalueTests
    , pandocTests
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

registerConstants :: forall a e. (Data a, LuaError e) => Proxy a -> LuaE e ()
registerConstants proxy =
  forM_ (constructors proxy) $ \c -> do
    pushString c
    setglobal (fromString c)

constructors :: forall a. Data a => Proxy a -> [String]
constructors _ = map showConstr . dataTypeConstrs . dataTypeOf @a $ undefined
