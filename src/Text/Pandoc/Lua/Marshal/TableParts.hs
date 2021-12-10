{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of types that are used exclusively
with tables.
-}
module Text.Pandoc.Lua.Marshal.TableParts
  ( peekCaption
  , pushCaption
  , peekColSpec
  , pushColSpec
  , peekRow
  , pushRow
  , peekTableBody
  , pushTableBody
  , peekTableFoot
  , pushTableFoot
  , peekTableHead
  , pushTableHead
  ) where

import Control.Applicative (optional)
import Control.Monad ((<$!>))
import HsLua
import Text.Pandoc.Lua.Marshal.Alignment (peekAlignment, pushAlignment)
import Text.Pandoc.Lua.Marshal.Attr (peekAttr, pushAttr)
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Block
  ( peekBlocksFuzzy, pushBlocks )
import Text.Pandoc.Lua.Marshal.Cell (peekCellFuzzy, pushCell)
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Inline
  ( peekInlinesFuzzy, pushInlines )
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Definition

-- | Push Caption element
pushCaption :: LuaError e => Caption -> LuaE e ()
pushCaption (Caption shortCaption longCaption) = do
  newtable
  addField "short" (maybe pushnil pushInlines shortCaption)
  addField "long" (pushBlocks longCaption)

-- | Peek Caption element
peekCaption :: LuaError e => Peeker e Caption
peekCaption = retrieving "Caption" . \idx -> do
  short <- optional $ peekFieldRaw peekInlinesFuzzy "short" idx
  long <- peekFieldRaw peekBlocksFuzzy "long" idx
  return $! Caption short long

-- | Push a ColSpec value as a pair of Alignment and ColWidth.
pushColSpec :: LuaError e => Pusher e ColSpec
pushColSpec = pushPair pushAlignment pushColWidth

-- | Peek a ColSpec value as a pair of Alignment and ColWidth.
peekColSpec :: LuaError e => Peeker e ColSpec
peekColSpec = peekPair peekAlignment peekColWidth

peekColWidth :: Peeker e ColWidth
peekColWidth = retrieving "ColWidth" . \idx -> do
  maybe ColWidthDefault ColWidth <$!> optional (peekRealFloat idx)

-- | Push a ColWidth value by pushing the width as a plain number, or
-- @nil@ for ColWidthDefault.
pushColWidth :: LuaError e => Pusher e ColWidth
pushColWidth = \case
  (ColWidth w)    -> push w
  ColWidthDefault -> pushnil

-- | Push a table row as a pair of attr and the list of cells.
pushRow :: LuaError e => Pusher e Row
pushRow (Row attr cells) =
  pushPair pushAttr (pushPandocList pushCell) (attr, cells)

-- | Push a table row from a pair of attr and the list of cells.
peekRow :: LuaError e => Peeker e Row
peekRow = (uncurry Row <$!>)
  . retrieving "Row"
  . peekPair peekAttr (peekList peekCellFuzzy)

-- | Pushes a 'TableBody' value as a Lua table with fields @attr@,
-- @row_head_columns@, @head@, and @body@.
pushTableBody :: LuaError e => Pusher e TableBody
pushTableBody (TableBody attr (RowHeadColumns rowHeadColumns) head' body) = do
    newtable
    addField "attr" (pushAttr attr)
    addField "row_head_columns" (pushIntegral rowHeadColumns)
    addField "head" (pushPandocList pushRow head')
    addField "body" (pushPandocList pushRow body)

-- | Retrieves a 'TableBody' value from a Lua table with fields @attr@,
-- @row_head_columns@, @head@, and @body@.
peekTableBody :: LuaError e => Peeker e TableBody
peekTableBody = fmap (retrieving "TableBody")
  . typeChecked "table" istable
  $ \idx -> TableBody
  <$!> peekFieldRaw peekAttr "attr" idx
  <*>  peekFieldRaw (fmap RowHeadColumns . peekIntegral) "row_head_columns" idx
  <*>  peekFieldRaw (peekList peekRow) "head" idx
  <*>  peekFieldRaw (peekList peekRow) "body" idx

-- | Push a table head value as the pair of its Attr and rows.
pushTableHead :: LuaError e => Pusher e TableHead
pushTableHead (TableHead attr rows) =
  pushPair pushAttr (pushPandocList pushRow) (attr, rows)

-- | Peek a table head value from a pair of Attr and rows.
peekTableHead :: LuaError e => Peeker e TableHead
peekTableHead = (uncurry TableHead <$!>)
  . retrieving "TableHead"
  . peekPair peekAttr (peekList peekRow)

-- | Pushes a 'TableFoot' value as a pair of the Attr value and the list
-- of table rows.
pushTableFoot :: LuaError e => Pusher e TableFoot
pushTableFoot (TableFoot attr rows) =
  pushPair pushAttr (pushPandocList pushRow) (attr, rows)

-- | Retrieves a 'TableFoot' value from a pair containing an Attr value
-- and a list of table rows.
peekTableFoot :: LuaError e => Peeker e TableFoot
peekTableFoot = (uncurry TableFoot <$!>)
  . retrieving "TableFoot"
  . peekPair peekAttr (peekList peekRow)

-- | Add a value to the table at the top of the stack at a string-index.
addField :: LuaError e => Name -> LuaE e () -> LuaE e ()
addField key pushValue = do
  pushName key
  pushValue
  rawset (nth 3)
