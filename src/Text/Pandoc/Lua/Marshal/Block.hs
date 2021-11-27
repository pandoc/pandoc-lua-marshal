{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{- |

Marshal values of types that make up 'Block' elements.
-}
module Text.Pandoc.Lua.Marshal.Block
  ( -- * Single Block elements
    peekBlock
  , peekBlockFuzzy
  , pushBlock
    -- * List of Blocks
  , peekBlocks
  , peekBlocksFuzzy
  , pushBlocks
    -- * Constructors
  , blockConstructors
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Catch (throwM)
import Control.Monad ((<$!>))
import Data.Data (showConstr, toConstr)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import HsLua hiding (Div)
import Text.Pandoc.Lua.Marshal.Attr (peekAttr, pushAttr)
import Text.Pandoc.Lua.Marshal.Content
  ( Content (..), contentTypeDescription, peekContent, pushContent
  , peekDefinitionItem )
import Text.Pandoc.Lua.Marshal.Format (peekFormat, pushFormat)
import Text.Pandoc.Lua.Marshal.Inline (peekInlinesFuzzy)
import Text.Pandoc.Lua.Marshal.List (newListMetatable, pushPandocList)
import Text.Pandoc.Lua.Marshal.ListAttributes (peekListAttributes, pushListAttributes)
import Text.Pandoc.Lua.Marshal.TableParts
  ( peekCaption, pushCaption
  , peekColSpec, pushColSpec
  , peekTableBody, pushTableBody
  , peekTableFoot, pushTableFoot
  , peekTableHead, pushTableHead
  )
import Text.Pandoc.Definition

-- | Pushes an Block value as userdata object.
pushBlock :: LuaError e => Pusher e Block
pushBlock = pushUD typeBlock
{-# INLINE pushBlock #-}

-- | Retrieves an Block value.
peekBlock :: LuaError e => Peeker e Block
peekBlock = peekUD typeBlock
{-# INLINE peekBlock #-}

-- | Retrieves a list of Block values.
peekBlocks :: LuaError e
           => Peeker e [Block]
peekBlocks = peekList peekBlock
{-# INLINABLE peekBlocks #-}

-- | Pushes a list of Block values.
pushBlocks :: LuaError e
           => Pusher e [Block]
pushBlocks xs = do
  pushList pushBlock xs
  newListMetatable "Blocks" $
    pure ()
  setmetatable (nth 2)
{-# INLINABLE pushBlocks #-}

-- | Try extra hard to retrieve an Block value from the stack. Treats
-- bare strings as @Str@ values.
peekBlockFuzzy :: LuaError e
               => Peeker e Block
peekBlockFuzzy = choice
  [ peekBlock
  , \idx -> Plain <$!> peekInlinesFuzzy idx
  ]
{-# INLINABLE peekBlockFuzzy #-}

-- | Try extra-hard to return the value at the given index as a list of
-- inlines.
peekBlocksFuzzy :: LuaError e
                => Peeker e [Block]
peekBlocksFuzzy = choice
  [ peekList peekBlockFuzzy
  , (<$!>) pure . peekBlockFuzzy
  ]
{-# INLINABLE peekBlocksFuzzy #-}

-- | Block object type.
typeBlock :: forall e. LuaError e => DocumentedType e Block
typeBlock = deftype "Block"
  [ operation Eq $ lambda
    ### liftPure2 (==)
    <#> parameter peekBlockFuzzy "Block" "a" ""
    <#> parameter peekBlockFuzzy "Block" "b" ""
    =#> boolResult "whether the two values are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> udparam typeBlock "self" ""
    =#> functionResult pushString "string" "Haskell representation"
  ]
  [ possibleProperty "attr" "element attributes"
      (pushAttr, \case
          CodeBlock attr _     -> Actual attr
          Div attr _           -> Actual attr
          Header _ attr _      -> Actual attr
          Table attr _ _ _ _ _ -> Actual attr
          _                    -> Absent)
      (peekAttr, \case
          CodeBlock _ code     -> Actual . flip CodeBlock code
          Div _ blks           -> Actual . flip Div blks
          Header lvl _ blks    -> Actual . (\attr -> Header lvl attr blks)
          Table _ c cs h bs f  -> Actual . (\attr -> Table attr c cs h bs f)
          _                    -> const Absent)
  , possibleProperty "bodies" "table bodies"
      (pushPandocList pushTableBody, \case
          Table _ _ _ _ bs _ -> Actual bs
          _                  -> Absent)
      (peekList peekTableBody, \case
          Table attr c cs h _ f -> Actual . (\bs -> Table attr c cs h bs f)
          _                     -> const Absent)
  , possibleProperty "caption" "element caption"
      (pushCaption, \case {Table _ capt _ _ _ _ -> Actual capt; _ -> Absent})
      (peekCaption, \case
          Table attr _ cs h bs f -> Actual . (\c -> Table attr c cs h bs f)
          _                      -> const Absent)
  , possibleProperty "colspecs" "column alignments and widths"
      (pushPandocList pushColSpec, \case
          Table _ _ cs _ _ _     -> Actual cs
          _                      -> Absent)
      (peekList peekColSpec, \case
          Table attr c _ h bs f  -> Actual . (\cs -> Table attr c cs h bs f)
          _                      -> const Absent)
  , possibleProperty "content" "element content"
      (pushContent, getBlockContent)
      (peekContent, setBlockContent (Proxy @e))
  , possibleProperty "foot" "table foot"
      (pushTableFoot, \case {Table _ _ _ _ _ f -> Actual f; _ -> Absent})
      (peekTableFoot, \case
          Table attr c cs h bs _ -> Actual . Table attr c cs h bs
          _                      -> const Absent)
  , possibleProperty "format" "format of raw content"
      (pushFormat, \case {RawBlock f _ -> Actual f; _ -> Absent})
      (peekFormat, \case
          RawBlock _ txt -> Actual . (`RawBlock` txt)
          _              -> const Absent)
  , possibleProperty "head" "table head"
      (pushTableHead, \case {Table _ _ _ h _ _ -> Actual h; _ -> Absent})
      (peekTableHead, \case
          Table attr c cs _ bs f  -> Actual . (\h -> Table attr c cs h bs f)
          _                       -> const Absent)
  , possibleProperty "level" "heading level"
      (pushIntegral, \case {Header lvl _ _ -> Actual lvl; _ -> Absent})
      (peekIntegral, \case
          Header _ attr inlns -> Actual . \lvl -> Header lvl attr inlns
          _                   -> const Absent)
  , possibleProperty "listAttributes" "ordered list attributes"
      (pushListAttributes, \case
          OrderedList listAttr _ -> Actual listAttr
          _                      -> Absent)
      (peekListAttributes, \case
          OrderedList _ content -> Actual . (`OrderedList` content)
          _                     -> const Absent)
  , possibleProperty "text" "text contents"
      (pushText, getBlockText)
      (peekText, setBlockText)

  , readonly "tag" "type of Block"
      (pushString, showConstr . toConstr )

  , alias "t" "tag" ["tag"]
  , alias "c" "content" ["content"]
  , alias "identifier" "element identifier"       ["attr", "identifier"]
  , alias "classes"    "element classes"          ["attr", "classes"]
  , alias "attributes" "other element attributes" ["attr", "attributes"]
  , alias "start"      "ordered list start number" ["listAttributes", "start"]
  , alias "style"      "ordered list style"       ["listAttributes", "style"]
  , alias "delimiter"  "numbering delimiter"      ["listAttributes", "delimiter"]

  , method $ defun "clone"
    ### return
    <#> parameter peekBlock "Block" "block" "self"
    =#> functionResult pushBlock "Block" "cloned Block"

  , method $ defun "show"
    ### liftPure show
    <#> parameter peekBlock "Block" "self" ""
    =#> functionResult pushString "string" "Haskell string representation"
  ]
 where
  boolResult = functionResult pushBool "boolean"

getBlockContent :: Block -> Possible Content
getBlockContent = \case
  -- inline content
  Para inlns          -> Actual $ ContentInlines inlns
  Plain inlns         -> Actual $ ContentInlines inlns
  Header _ _ inlns    -> Actual $ ContentInlines inlns
  -- inline content
  BlockQuote blks     -> Actual $ ContentBlocks blks
  Div _ blks          -> Actual $ ContentBlocks blks
  -- lines content
  LineBlock lns       -> Actual $ ContentLines lns
  -- list items content
  BulletList itms     -> Actual $ ContentListItems itms
  OrderedList _ itms  -> Actual $ ContentListItems itms
  -- definition items content
  DefinitionList itms -> Actual $ ContentDefItems itms
  _                   -> Absent

setBlockContent :: forall e. LuaError e
                => Proxy e -> Block -> Content -> Possible Block
setBlockContent _ = \case
  -- inline content
  Para _           -> Actual . Para . inlineContent
  Plain _          -> Actual . Plain . inlineContent
  Header attr lvl _ -> Actual . Header attr lvl . inlineContent
  -- block content
  BlockQuote _     -> Actual . BlockQuote . blockContent
  Div attr _       -> Actual . Div attr . blockContent
  -- lines content
  LineBlock _      -> Actual . LineBlock . lineContent
  -- list items content
  BulletList _     -> Actual . BulletList . listItemContent
  OrderedList la _ -> Actual . OrderedList la . listItemContent
  -- definition items content
  DefinitionList _ -> Actual . DefinitionList . defItemContent
  _                -> const Absent
 where
    inlineContent = \case
      ContentInlines inlns -> inlns
      c -> throwM . luaException @e $
           "expected Inlines, got " <> contentTypeDescription c
    blockContent = \case
      ContentBlocks blks   -> blks
      ContentInlines inlns -> [Plain inlns]
      c -> throwM . luaException @e $
           "expected Blocks, got " <> contentTypeDescription c
    lineContent = \case
      ContentLines lns     -> lns
      c -> throwM . luaException @e $
           "expected list of lines (Inlines), got " <> contentTypeDescription c
    defItemContent = \case
      ContentDefItems itms -> itms
      c -> throwM . luaException @e $
           "expected definition items, got " <> contentTypeDescription c
    listItemContent = \case
      ContentBlocks blks    -> [blks]
      ContentLines lns      -> map ((:[]) . Plain) lns
      ContentListItems itms -> itms
      c -> throwM . luaException @e $
           "expected list of items, got " <> contentTypeDescription c

getBlockText :: Block -> Possible Text
getBlockText = \case
  CodeBlock _ lst -> Actual lst
  RawBlock _ raw  -> Actual raw
  _               -> Absent

setBlockText :: Block -> Text -> Possible Block
setBlockText = \case
  CodeBlock attr _ -> Actual . CodeBlock attr
  RawBlock f _     -> Actual . RawBlock f
  _                -> const Absent

-- | Constructor functions for 'Block' elements.
blockConstructors :: LuaError e => [DocumentedFunction e]
blockConstructors =
  [ defun "BlockQuote"
    ### liftPure BlockQuote
    <#> blocksParam
    =#> blockResult "BlockQuote element"

  , defun "BulletList"
    ### liftPure BulletList
    <#> blockItemsParam "list items"
    =#> blockResult "BulletList element"

  , defun "CodeBlock"
    ### liftPure2 (\code mattr -> CodeBlock (fromMaybe nullAttr mattr) code)
    <#> textParam "text" "code block content"
    <#> optAttrParam
    =#> blockResult "CodeBlock element"

  , defun "DefinitionList"
    ### liftPure DefinitionList
    <#> parameter (choice
                   [ peekList peekDefinitionItem
                   , \idx -> (:[]) <$!> peekDefinitionItem idx
                   ])
                  "{{Inlines, {Blocks,...}},...}"
                  "content" "definition items"
    =#> blockResult "DefinitionList element"

  , defun "Div"
    ### liftPure2 (\content mattr -> Div (fromMaybe nullAttr mattr) content)
    <#> blocksParam
    <#> optAttrParam
    =#> blockResult "Div element"

  , defun "Header"
    ### liftPure3 (\lvl content mattr ->
                     Header lvl (fromMaybe nullAttr mattr) content)
    <#> parameter peekIntegral "integer" "level" "heading level"
    <#> parameter peekInlinesFuzzy "Inlines" "content" "inline content"
    <#> optAttrParam
    =#> blockResult "Header element"

  , defun "HorizontalRule"
    ### return HorizontalRule
    =#> blockResult "HorizontalRule element"

  , defun "LineBlock"
    ### liftPure LineBlock
    <#> parameter (peekList peekInlinesFuzzy) "{Inlines,...}" "content" "lines"
    =#> blockResult "LineBlock element"

  , defun "Null"
    ### return Null
    =#> blockResult "Null element"

  , defun "OrderedList"
    ### liftPure2 (\items mListAttrib ->
                     let defListAttrib = (1, DefaultStyle, DefaultDelim)
                     in OrderedList (fromMaybe defListAttrib mListAttrib) items)
    <#> blockItemsParam "ordered list items"
    <#> optionalParameter peekListAttributes "ListAttributes" "listAttributes"
                          "specifier for the list's numbering"
    =#> blockResult "OrderedList element"

  , defun "Para"
    ### liftPure Para
    <#> parameter peekInlinesFuzzy "Inlines" "content" "paragraph content"
    =#> blockResult "Para element"

  , defun "Plain"
    ### liftPure Plain
    <#> parameter peekInlinesFuzzy "Inlines" "content" "paragraph content"
    =#> blockResult "Plain element"

  , defun "RawBlock"
    ### liftPure2 RawBlock
    <#> parameter peekFormat "Format" "format" "format of content"
    <#> parameter peekText "string" "text" "raw content"
    =#> blockResult "RawBlock element"

  , defun "Table"
    ### (\capt colspecs thead tbodies tfoot mattr ->
           let attr = fromMaybe nullAttr mattr
           in return $! attr `seq` capt `seq` colspecs `seq` thead `seq` tbodies
              `seq` tfoot `seq` Table attr capt colspecs thead tbodies tfoot)
    <#> parameter peekCaption "Caption" "caption" "table caption"
    <#> parameter (peekList peekColSpec) "{ColSpec,...}" "colspecs"
                  "column alignments and widths"
    <#> parameter peekTableHead "TableHead" "head" "table head"
    <#> parameter (peekList peekTableBody) "{TableBody,...}" "bodies"
                  "table bodies"
    <#> parameter peekTableFoot "TableFoot" "foot" "table foot"
    <#> optAttrParam
    =#> blockResult "Table element"
  ]
 where
  blockResult = functionResult pushBlock "Block"
  blocksParam = parameter peekBlocksFuzzy "Blocks" "content" "block content"
  blockItemsParam = parameter peekItemsFuzzy "List of Blocks" "content"
  peekItemsFuzzy idx = peekList peekBlocksFuzzy idx
    <|> ((:[]) <$!> peekBlocksFuzzy idx)

textParam :: Text -> Text -> Parameter e Text
textParam = parameter peekText "string"

optAttrParam :: LuaError e => Parameter e (Maybe Attr)
optAttrParam = optionalParameter peekAttr "attr" "Attr" "additional attributes"
