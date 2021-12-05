{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{- |
Copyright  : © 2021 Albert Krewinkel
License    : MIT
Maintainer : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions Lua filters, i.e., tables containing
functions to be called on specific elements.
-}
module Text.Pandoc.Lua.Marshal.Filter
  ( -- * Filters
    peekFilter'
    -- * Individual filter functions
  , peekFilterFunction
  , pushFilterFunction
  ) where

import Control.Monad ((<$!>))
import Data.Foldable (foldrM)
import Data.Map (Map)
import HsLua
import qualified Data.Map.Strict as Map

-- | Filter function stored in the registry
newtype FilterFunction = FilterFunction Reference

-- | Pushes a filter function to the stack.
--
-- Filter functions are stored in the registry and retrieved from there.
pushFilterFunction :: LuaError e => FilterFunction -> LuaE e ()
pushFilterFunction (FilterFunction fnRef) =
  getref registryindex fnRef

-- | Retrieves a filter function from the stack.
--
-- The value at the given index must be a function. It is stored in the
-- Lua registry.
peekFilterFunction :: Peeker e FilterFunction
peekFilterFunction = typeChecked "function" isfunction $ \idx -> liftLua $ do
  pushvalue idx
  FilterFunction <$> ref registryindex


-- | Collection of filter functions (at most one function per element
-- constructor)
newtype Filter = Filter (Map Name FilterFunction)

-- | Retrieves a `Filter` object from the stack.
peekFilter' :: LuaError e => [Name] -> Peeker e Filter
peekFilter' constructors idx = do
  let go constr acc = liftLua $ do
        _ <- getfield idx constr
        runPeek (peekFilterFunction top `lastly` pop 1) >>= \case
          Success fn -> pure $ Map.insert constr fn acc
          Failure {} -> pure acc
  Filter <$!> foldrM go Map.empty constructors
