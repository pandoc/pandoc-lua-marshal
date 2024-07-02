{- |
Copyright               : © 2021-2024 Albert Krewinkel
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
  ) where

import HsLua
import Text.Pandoc.Definition

-- | Push a table cell as a table with fields @attr@, @alignment@,
-- @row_span@, @col_span@, and @contents@.
pushCell :: LuaError e => Cell -> LuaE e ()

-- | Retrieves a 'Cell' object from the stack.
peekCell :: LuaError e => Peeker e Cell

-- | Retrieves a 'Cell' from the stack, accepting either a 'pandoc Cell'
-- userdata object or a table with fields @attr@, @alignment@, @row_span@,
-- @col_span@, and @contents@.
peekCellFuzzy :: LuaError e => Peeker e Cell

-- | Cell object type.
typeCell :: LuaError e => DocumentedType e Cell

-- | Constructor function for 'Cell' values.
mkCell :: LuaError e => DocumentedFunction e
