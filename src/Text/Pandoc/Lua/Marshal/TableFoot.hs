{-# LANGUAGE OverloadedStrings    #-}
{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'TableFoot' values.
-}
module Text.Pandoc.Lua.Marshal.TableFoot
  ( peekTableFoot
  , pushTableFoot
  , typeTableFoot
  , mkTableFoot
  ) where

import Control.Applicative (optional)
import Data.Maybe (fromMaybe)
import HsLua
import Text.Pandoc.Lua.Marshal.Attr (peekAttr, pushAttr)
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Lua.Marshal.Row (peekRowFuzzy, pushRow)
import Text.Pandoc.Definition

-- | Push a TableFoot as a userdata value.
pushTableFoot :: LuaError e => TableFoot -> LuaE e ()
pushTableFoot = pushUD typeTableFoot

-- | Retrieves a 'Cell' from the stack.
peekTableFoot :: LuaError e => Peeker e TableFoot
peekTableFoot = peekUD typeTableFoot

-- | Row object type.
typeTableFoot :: LuaError e => DocumentedType e TableFoot
typeTableFoot = deftype "pandoc TableFoot"
  [ operation Eq $ defun "__eq"
     ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
     <#> parameter (optional . peekTableFoot) "TableFoot" "self" ""
     <#> parameter (optional . peekTableFoot) "any" "object" ""
     =#> functionResult pushBool "boolean" "true iff the two values are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> parameter peekTableFoot "TableFoot" "self" ""
    =#> functionResult pushString "string" "native Haskell representation"
  ]
  [ property "attr" "table foot attributes"
      (pushAttr, \(TableFoot attr _) -> attr)
      (peekAttr, \(TableFoot _ cells) attr ->
                   TableFoot attr cells)
  , property "rows" "footer rows"
      (pushPandocList pushRow, \(TableFoot _ rows) -> rows)
      (peekList peekRowFuzzy, \(TableFoot attr _) rows ->
                                TableFoot attr rows)

  , alias "identifier" "cell ID"         ["attr", "identifier"]
  , alias "classes"    "cell classes"    ["attr", "classes"]
  , alias "attributes" "cell attributes" ["attr", "attributes"]

  , method $ defun "clone"
    ### return
    <#> parameter peekTableFoot "TableFoot" "self" ""
    =#> functionResult pushTableFoot "TableFoot" "cloned object"
  ]

-- | Constructor function for 'Row' values.
mkTableFoot :: LuaError e => DocumentedFunction e
mkTableFoot = defun "TableFoot"
  ### liftPure2 (\mCells mAttr -> TableFoot
                  (fromMaybe nullAttr mAttr)
                  (fromMaybe [] mCells))
  <#> optionalParameter (peekList peekRowFuzzy) "{Row,...}" "rows"
        "footer rows"
  <#> optionalParameter peekAttr "Attr" "attr" "table foot attributes"
  =#> functionResult pushTableFoot "TableFoot" "new TableFoot object"
