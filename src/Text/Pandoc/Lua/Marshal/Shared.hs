{-# LANGUAGE FlexibleContexts     #-}
{- |
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Shared functions used in multiple types.
-}
module Text.Pandoc.Lua.Marshal.Shared
  ( -- * Walking
    walkBlocksAndInlines
  ) where

import Control.Monad ((>=>))
import HsLua (LuaE, LuaError)
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Block
  ( walkBlockSplicing, walkBlocksStraight )
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Cell (walkCellSplicing)
import Text.Pandoc.Lua.Marshal.Filter (Filter)
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Inline
  ( walkInlineSplicing, walkInlinesStraight )
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Walk (SpliceList, Walkable)

-- | Walk blocks and inlines.
walkBlocksAndInlines :: (LuaError e,
                         Walkable (SpliceList Block) a,
                         Walkable (SpliceList Cell) a,
                         Walkable (SpliceList Inline) a,
                         Walkable [Block] a,
                         Walkable [Inline] a)
                     => Filter
                     -> a -> LuaE e a
walkBlocksAndInlines f =
      walkInlineSplicing f
  >=> walkInlinesStraight f
  >=> walkBlockSplicing f
  >=> walkBlocksStraight f
  >=> walkCellSplicing f
