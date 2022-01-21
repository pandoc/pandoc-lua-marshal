{-# LANGUAGE OverloadedStrings    #-}
{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'TableHead' values.
-}
module Text.Pandoc.Lua.Marshal.TableHead
  ( peekTableHead
  , pushTableHead
  , typeTableHead
  , mkTableHead
  ) where

import Control.Applicative (optional)
import Data.Maybe (fromMaybe)
import HsLua
import Text.Pandoc.Lua.Marshal.Attr (peekAttr, pushAttr)
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Lua.Marshal.Row (peekRowFuzzy, pushRow)
import Text.Pandoc.Definition

-- | Push a TableHead as a userdata value.
pushTableHead :: LuaError e => TableHead -> LuaE e ()
pushTableHead = pushUD typeTableHead

-- | Retrieves a 'Cell' from the stack.
peekTableHead :: LuaError e => Peeker e TableHead
peekTableHead = peekUD typeTableHead

-- | Row object type.
typeTableHead :: LuaError e => DocumentedType e TableHead
typeTableHead = deftype "pandoc TableHead"
  [ operation Eq $ defun "__eq"
     ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
     <#> parameter (optional . peekTableHead) "TableHead" "self" ""
     <#> parameter (optional . peekTableHead) "any" "object" ""
     =#> functionResult pushBool "boolean" "true iff the two values are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> parameter peekTableHead "TableHead" "self" ""
    =#> functionResult pushString "string" "native Haskell representation"
  ]
  [ property "attr" "table head attributes"
      (pushAttr, \(TableHead attr _) -> attr)
      (peekAttr, \(TableHead _ cells) attr ->
                   TableHead attr cells)
  , property "rows" "header rows"
      (pushPandocList pushRow, \(TableHead _ rows) -> rows)
      (peekList peekRowFuzzy, \(TableHead attr _) rows ->
                                TableHead attr rows)

  , alias "identifier" "cell ID"         ["attr", "identifier"]
  , alias "classes"    "cell classes"    ["attr", "classes"]
  , alias "attributes" "cell attributes" ["attr", "attributes"]

  , method $ defun "clone"
    ### return
    <#> parameter peekTableHead "TableHead" "self" ""
    =#> functionResult pushTableHead "TableHead" "cloned object"
  ]

-- | Constructor function for 'Row' values.
mkTableHead :: LuaError e => DocumentedFunction e
mkTableHead = defun "TableHead"
  ### liftPure2 (\mRows mAttr -> TableHead
                  (fromMaybe nullAttr mAttr)
                  (fromMaybe [] mRows))
  <#> opt (parameter (peekList peekRowFuzzy) "{Row,...}" "rows" "header rows")
  <#> opt (parameter peekAttr "Attr" "attr" "table head attributes")
  =#> functionResult pushTableHead "TableHead" "new TableHead object"
