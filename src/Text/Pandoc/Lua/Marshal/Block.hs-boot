{-# LANGUAGE FlexibleContexts     #-}
module Text.Pandoc.Lua.Marshal.Block
  ( peekBlocksFuzzy
  , pushBlocks
  , walkBlockSplicing
  , walkBlocks
  ) where

import HsLua
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Marshal.Filter (Filter)
import Text.Pandoc.Lua.Walk (SpliceList, Walkable)

-- | Pushes a list of Block values.
pushBlocks :: LuaError e => Pusher e [Block]

peekBlocksFuzzy :: LuaError e => Peeker e [Block]

walkBlockSplicing :: (LuaError e, Walkable (SpliceList Block) a)
                  => Filter -> a -> LuaE e a

walkBlocks :: (LuaError e, Walkable [Block] a)
           => Filter -> a -> LuaE e a