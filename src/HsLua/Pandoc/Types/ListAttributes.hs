{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions and constructor for 'ListAttributes'
values.
-}
module HsLua.Pandoc.Types.ListAttributes
  ( typeListAttributes
  , peekListAttributes
  , pushListAttributes
  , mkListAttributes
  ) where

import Data.Maybe (fromMaybe)
import HsLua
import Text.Pandoc.Definition
  ( ListAttributes, ListNumberStyle (..), ListNumberDelim (..))

-- | 'ListAttributes' Lua object type.
typeListAttributes :: LuaError e => DocumentedType e ListAttributes
typeListAttributes = deftype "ListAttributes"
  [ operation Eq $ lambda
    ### liftPure2 (==)
    <#> parameter peekListAttributes "a" "ListAttributes" ""
    <#> parameter peekListAttributes "b" "ListAttributes" ""
    =#> functionResult pushBool "boolean" "whether the two are equal"
  ]
  [ property "start" "number of the first list item"
      (pushIntegral, \(start,_,_) -> start)
      (peekIntegral, \(_,style,delim) -> (,style,delim))
  , property "style" "style used for list numbering"
      (pushString . show, \(_,classes,_) -> classes)
      (peekRead, \(start,_,delim) -> (start,,delim))
  , property "delimiter" "delimiter of list numbers"
      (pushString . show, \(_,_,delim) -> delim)
      (peekRead, \(start,style,_) -> (start,style,))
  , method $ defun "clone"
    ### return
    <#> udparam typeListAttributes "a" ""
    =#> functionResult (pushUD typeListAttributes) "ListAttributes"
          "cloned ListAttributes value"
  ]

-- | Pushes a 'ListAttributes' value as userdata object.
pushListAttributes :: LuaError e => Pusher e ListAttributes
pushListAttributes = pushUD typeListAttributes

-- | Retrieve a 'ListAttributes' triple, either from userdata or from a
-- Lua tuple.
peekListAttributes :: LuaError e => Peeker e ListAttributes
peekListAttributes = retrieving "ListAttributes" . choice
  [ peekUD typeListAttributes
  , peekTriple peekIntegral peekRead peekRead
  ]

-- | Constructor for a new 'ListAttributes' value.
mkListAttributes :: LuaError e => DocumentedFunction e
mkListAttributes = defun "ListAttributes"
  ### liftPure3 (\mstart mstyle mdelim ->
                   ( fromMaybe 1 mstart
                   , fromMaybe DefaultStyle mstyle
                   , fromMaybe DefaultDelim mdelim
                   ))
  <#> optionalParameter peekIntegral "integer" "start" "number of first item"
  <#> optionalParameter peekRead "string" "style" "list numbering style"
  <#> optionalParameter peekRead "string" "delimiter" "list number delimiter"
  =#> functionResult pushListAttributes "ListAttributes" "new ListAttributes"
  #? "Creates a new ListAttributes object."
