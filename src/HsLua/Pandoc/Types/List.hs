{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions and constructor for 'ListAttributes'
values.
-}
module HsLua.Pandoc.Types.List
  ( pushPandocList
  , luaopen_list_ptr
  , pushListModule
  -- , mkList
  ) where

import HsLua

-- | Pushes a list as a numerically-indexed Lua table, and sets a
-- metatable that offers a number of convenience functions.
pushPandocList :: LuaError e => Pusher e a -> Pusher e [a]
pushPandocList = pushList

foreign import ccall unsafe "listmod.c &luaopen_list"
  luaopen_list_ptr :: CFunction

pushListModule :: LuaError e => LuaE e ()
pushListModule = do
  pushcfunction luaopen_list_ptr
  call 0 1

-- mkList :: LuaError e => DocumentedFunction e
-- mkList = DocumentedFunction
