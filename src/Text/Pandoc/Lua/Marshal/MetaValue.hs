{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'MetaValue' elements.
-}
module Text.Pandoc.Lua.Marshal.MetaValue
  ( peekMetaValue
  , pushMetaValue
  , metaValueConstructors
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad ((<$!>))
import HsLua
import Text.Pandoc.Lua.Marshal.Block
  ( peekBlock, peekBlocks, peekBlocksFuzzy, pushBlocks )
import Text.Pandoc.Lua.Marshal.Inline
  ( peekInline, peekInlines, peekInlinesFuzzy, pushInlines )
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Definition (MetaValue (..))

-- | Push a 'MetaValue' element to the top of the Lua stack.
pushMetaValue :: LuaError e => Pusher e MetaValue
pushMetaValue = \case
  MetaBlocks blcks  -> pushBlocks blcks
  MetaBool bool     -> pushBool bool
  MetaInlines inlns -> pushInlines inlns
  MetaList metalist -> pushPandocList pushMetaValue metalist
  MetaMap metamap   -> pushMap pushText pushMetaValue metamap
  MetaString t      -> pushText t

-- | Retrieves the value at the given stack index as 'MetaValue'.
peekMetaValue :: forall e. LuaError e => Peeker e MetaValue
peekMetaValue = retrieving "MetaValue" . \idx -> do
  -- Get the contents of an AST element.

  liftLua (ltype idx) >>= \case
    TypeBoolean -> MetaBool <$!> peekBool idx

    TypeString  -> MetaString <$!> peekText idx

    TypeUserdata -> -- Allow singleton Inline or Block elements
      (MetaInlines . (:[]) <$!> peekInline idx) <|>
      (MetaBlocks . (:[]) <$!> peekBlock idx)

    TypeTable   -> optional (getName idx) >>= \case
      Just "Inlines" -> MetaInlines <$!> peekInlinesFuzzy idx
      Just "Blocks"  -> MetaBlocks  <$!> peekBlocksFuzzy idx
      Just "List"    -> MetaList <$!> peekList peekMetaValue idx
      _ -> do
        -- no meta value tag given, try to guess.
        len <- liftLua $ rawlen idx
        if len <= 0
          then MetaMap <$!> peekMap peekText peekMetaValue idx
          else  (MetaInlines <$!> peekInlines idx)
            <|> (MetaBlocks <$!> peekBlocks idx)
            <|> (MetaList <$!> peekList peekMetaValue idx)

    _ -> failPeek "could not get meta value"

 where
  getName idx = liftLua (getmetafield idx "__name") >>= \case
    TypeNil -> failPeek "no name"
    _ -> peekName idx `lastly` pop 1


-- | Constructor functions for 'MetaValue' elements.
metaValueConstructors :: LuaError e => [DocumentedFunction e]
metaValueConstructors =
  [ defun "MetaBlocks"
    ### liftPure MetaBlocks
    <#> parameter peekBlocksFuzzy "Blocks" "content" "block content"
    =#> functionResult pushMetaValue "Blocks" "list of Block elements"

  , defun "MetaBool"
    ### liftPure MetaBool
    <#> parameter peekBool "boolean" "bool" "true or false"
    =#> functionResult pushMetaValue "boolean" "input, unchanged"

  , defun "MetaInlines"
    ### liftPure MetaInlines
    <#> parameter peekInlinesFuzzy "Inlines" "inlines" "inline elements"
    =#> functionResult pushMetaValue "Inlines" "list of Inline elements"

  , defun "MetaList"
    ### liftPure MetaList
    <#> parameter (peekList peekMetaValue) "MetaValue|{MetaValue,...}"
          "values" "value, or list of values"
    =#> functionResult pushMetaValue "List" "list of meta values"

  , defun "MetaMap"
    ### liftPure MetaMap
    <#> parameter (peekMap peekText peekMetaValue) "table" "map"
          "string-indexed table"
    =#> functionResult pushMetaValue "table" "map of meta values"

  , defun "MetaString"
    ### liftPure MetaString
    <#> parameter peekText "string" "s" "string value"
    =#> functionResult pushMetaValue "string" "unchanged input"
  ]
