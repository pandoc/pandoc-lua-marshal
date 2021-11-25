{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
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
  ) where

import HsLua

-- | Pushes a list as a numerically-indexed Lua table, and sets a
-- metatable that offers a number of convenience functions.
pushPandocList :: LuaError e => Pusher e a -> Pusher e [a]
pushPandocList pushItem items = do
  pushList pushItem items
  getmetatable' "List" >>= \case
    TypeTable -> setmetatable (nth 2)
    _ -> failLua "List has not been initialized correctly."

foreign import ccall unsafe "listmod.c &luaopen_list"
  luaopen_list_ptr :: CFunction

-- foreign import capi unsafe "listmod.c value LIST_T"
--   c_list_t :: CString

pushListModule :: LuaError e => LuaE e ()
pushListModule = do
  pushcfunction luaopen_list_ptr
  call 0 1
