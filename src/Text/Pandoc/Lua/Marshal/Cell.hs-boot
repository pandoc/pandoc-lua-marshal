{-# LANGUAGE FlexibleContexts     #-}
{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of table 'Cell' values.
-}
module Text.Pandoc.Lua.Marshal.Cell
  ( peekCell
  , peekCellFuzzy
  , pushCell
  , typeCell
  , mkCell
    -- * Walking
  , walkCellSplicing
  ) where

import HsLua
import Text.Pandoc.Lua.Marshal.Filter (Filter)
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Walk (SpliceList, Walkable)

pushCell :: LuaError e => Cell -> LuaE e ()
peekCell :: LuaError e => Peeker e Cell
peekCellFuzzy :: LuaError e => Peeker e Cell

typeCell :: LuaError e => DocumentedType e Cell
mkCell   :: LuaError e => DocumentedFunction e

walkCellSplicing :: (LuaError e, Walkable (SpliceList Cell) a)
                 => Filter -> a -> LuaE e a
