module Text.Pandoc.Lua.Marshal.Block
  ( peekBlocksFuzzy
  , pushBlocks
  ) where

import HsLua
import Text.Pandoc.Definition

-- | Pushes a list of Block values.
pushBlocks :: LuaError e => Pusher e [Block]

peekBlocksFuzzy :: LuaError e => Peeker e [Block]
