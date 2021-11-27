{-# LANGUAGE OverloadedStrings    #-}
{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of table 'Cell' values.
-}
module Text.Pandoc.Lua.Marshal.Cell
  ( peekCell
  , pushCell
  ) where

import Control.Monad ((<$!>))
import HsLua
import Text.Pandoc.Lua.Marshal.Alignment (peekAlignment, pushAlignment)
import Text.Pandoc.Lua.Marshal.Attr (peekAttr, pushAttr)
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Block
  ( peekBlocksFuzzy, pushBlocks )
import Text.Pandoc.Definition (Cell (..), RowSpan (..), ColSpan (..))

-- | Push a table cell as a table with fields @attr@, @alignment@,
-- @row_span@, @col_span@, and @contents@.
pushCell :: LuaError e => Cell -> LuaE e ()
pushCell (Cell attr align (RowSpan rowSpan) (ColSpan colSpan) contents) = do
  newtable
  addField "attr" (pushAttr attr)
  addField "alignment" (pushAlignment align)
  addField "row_span" (pushIntegral rowSpan)
  addField "col_span" (pushIntegral colSpan)
  addField "contents" (pushBlocks contents)
 where addField key pusher = pushName key *> pusher *> rawset (nth 3)

-- | Retrieves a table 'Cell' from the stack.
peekCell :: LuaError e => Peeker e Cell
peekCell = fmap (retrieving "Cell")
  . typeChecked "table" istable
  $ \idx -> do
  attr <- peekFieldRaw peekAttr "attr" idx
  algn <- peekFieldRaw peekAlignment "alignment" idx
  rs   <- RowSpan <$!> peekFieldRaw peekIntegral "row_span" idx
  cs   <- ColSpan <$!> peekFieldRaw peekIntegral "col_span" idx
  blks <- peekFieldRaw peekBlocksFuzzy "contents" idx
  return $! Cell attr algn rs cs blks
