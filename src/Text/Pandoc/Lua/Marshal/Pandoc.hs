{-# LANGUAGE OverloadedStrings    #-}
{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'Pandoc' values.
-}
module Text.Pandoc.Lua.Marshal.Pandoc
  ( -- * Pandoc
    peekPandoc
  , pushPandoc
  , mkPandoc
    -- * Meta
  , peekMeta
  , pushMeta
  , mkMeta
  ) where

import Control.Applicative (optional)
import Control.Monad ((<$!>))
import Data.Maybe (fromMaybe)
import HsLua
import Text.Pandoc.Lua.Marshal.Block (peekBlocksFuzzy, pushBlocks)
import Text.Pandoc.Lua.Marshal.MetaValue (peekMetaValue, pushMetaValue)
import Text.Pandoc.Definition (Pandoc (..), Meta (..), nullMeta)

-- | Pushes a 'Pandoc' value as userdata.
pushPandoc :: LuaError e => Pusher e Pandoc
pushPandoc = pushUD typePandoc

-- | Retrieves a 'Pandoc' document from a userdata value.
peekPandoc :: LuaError e => Peeker e Pandoc
peekPandoc = retrieving "Pandoc" . peekUD typePandoc

-- | Pandoc object type.
typePandoc :: LuaError e => DocumentedType e Pandoc
typePandoc = deftype "Pandoc"
  [ operation Eq $ defun "__eq"
     ### liftPure2 (==)
     <#> parameter (optional . peekPandoc) "doc1" "pandoc" ""
     <#> parameter (optional . peekPandoc) "doc2" "pandoc" ""
     =#> functionResult pushBool "boolean" "true iff the two values are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> parameter peekPandoc "Pandoc" "doc" ""
    =#> functionResult pushString "string" "native Haskell representation"
  ]
  [ property "blocks" "list of blocks"
      (pushBlocks, \(Pandoc _ blks) -> blks)
      (peekBlocksFuzzy, \(Pandoc m _) blks -> Pandoc m blks)
  , property "meta" "document metadata"
      (pushMeta, \(Pandoc meta _) -> meta)
      (peekMeta, \(Pandoc _ blks) meta -> Pandoc meta blks)
  ]

-- | Pushes a 'Meta' value as a string-indexed table.
pushMeta :: LuaError e => Pusher e Meta
pushMeta (Meta mmap) = do
  pushMap pushText pushMetaValue mmap
  _ <- newmetatable "Meta"
  setmetatable (nth 2)

-- | Retrieves a 'Meta' value from a string-indexed table.
peekMeta :: LuaError e => Peeker e Meta
peekMeta idx = retrieving "Meta" $
  Meta <$!> peekMap peekText peekMetaValue idx

-- | Constructor function for 'Pandoc' values.
mkPandoc :: LuaError e => DocumentedFunction e
mkPandoc = defun "Pandoc"
  ### liftPure2 (\blocks mMeta -> Pandoc (fromMaybe nullMeta mMeta) blocks)
  <#> parameter peekBlocksFuzzy "Blocks" "blocks" "document contents"
  <#> optionalParameter peekMeta "Meta" "meta" "document metadata"
  =#> functionResult pushPandoc "Pandoc" "new Pandoc document"

-- | Constructor for 'Meta' values.
mkMeta :: LuaError e => DocumentedFunction e
mkMeta = defun "Meta"
  ### liftPure id
  <#> parameter peekMeta "table" "meta" "table containing meta information"
  =#> functionResult pushMeta "table" "new Meta table"
