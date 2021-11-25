{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Defines a helper type that can handle different types of 'Block' and
'Inline' element contents.
-}
module HsLua.Pandoc.Types.Content
  ( Content (..)
  , contentTypeDescription
  , peekContent
  , pushContent
  , peekDefinitionItem
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<$!>))
import HsLua
import {-# SOURCE #-} HsLua.Pandoc.Types.Block
  ( peekBlocksFuzzy, pushBlocks )
import {-# SOURCE #-} HsLua.Pandoc.Types.Inline
  ( peekInlinesFuzzy, pushInlines )
import HsLua.Pandoc.Types.List (pushPandocList)
import Text.Pandoc.Definition (Inline, Block)

--
-- Content
--

-- | Helper type to represent all the different types a `content`
-- attribute can have.
data Content
  = ContentBlocks [Block]
  | ContentInlines [Inline]
  | ContentLines [[Inline]]
  | ContentDefItems [([Inline], [[Block]])]
  | ContentListItems [[Block]]

-- | Gets the text property of an Inline, if present.
contentTypeDescription :: Content -> String
contentTypeDescription = \case
  ContentBlocks {}    -> "list of Block items"
  ContentInlines {}   -> "list of Inline items"
  ContentLines {}     -> "list of Inline lists (i.e., a list of lines)"
  ContentDefItems {}  -> "list of definition items items"
  ContentListItems {} -> "list items (i.e., list of list of Block elements)"

-- | Pushes the 'Content' to the stack.
pushContent :: LuaError e => Pusher e Content
pushContent = \case
  ContentBlocks blks    -> pushBlocks blks
  ContentInlines inlns  -> pushInlines inlns
  ContentLines lns      -> pushPandocList pushInlines lns
  ContentDefItems itms  -> pushPandocList pushDefinitionItem itms
  ContentListItems itms -> pushPandocList pushBlocks itms

-- | Gets a 'Content' element from the stack.
peekContent :: LuaError e => Peeker e Content
peekContent idx =
  (ContentInlines <$!> peekInlinesFuzzy idx) <|>
  (ContentLines  <$!> peekList peekInlinesFuzzy idx) <|>
  (ContentBlocks  <$!> peekBlocksFuzzy idx ) <|>
  (ContentListItems <$!> peekList peekBlocksFuzzy idx) <|>
  (ContentDefItems  <$!> peekList peekDefinitionItem idx)

-- | Retrieves a single definition item from the stack; it is expected
-- to be a pair of a list of inlines and a list of list of blocks. Uses
-- fuzzy parsing, i.e., tries hard to convert mismatching types into the
-- expected result.
peekDefinitionItem :: LuaError e => Peeker e ([Inline], [[Block]])
peekDefinitionItem = peekPair peekInlinesFuzzy $ choice
  [ peekList peekBlocksFuzzy
  , \idx -> (:[]) <$!> peekBlocksFuzzy idx
  ]

-- | Pushes a single definition items on the stack.
pushDefinitionItem :: LuaError e => Pusher e ([Inline], [[Block]])
pushDefinitionItem = pushPair pushInlines
                              (pushPandocList pushBlocks)
