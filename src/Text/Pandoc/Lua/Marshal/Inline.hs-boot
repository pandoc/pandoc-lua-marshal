module Text.Pandoc.Lua.Marshal.Inline
  ( peekInlinesFuzzy
  , pushInlines
  )
where

import HsLua
import Text.Pandoc.Definition (Inline)

peekInlinesFuzzy :: LuaError e => Peeker e [Inline]
pushInlines :: LuaError e => Pusher e [Inline]