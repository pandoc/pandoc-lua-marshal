{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{- |

Marshal values of types that make up 'Inline' elements.
-}
module HsLua.Pandoc.Types.Inline
  ( -- * Single Inline elements
    peekInline
  , peekInlineFuzzy
  , pushInline
    -- * List of Inlines
  , peekInlines
  , peekInlinesFuzzy
  , pushInlines
  ) where

import Control.Monad ((<$!>))
import HsLua
import HsLua.Pandoc.Types.List (pushPandocList)
import Text.Pandoc.Definition (Inline (Str, Space))
import qualified Text.Pandoc.Builder as B

-- | Pushes an Inline value as userdata object.
pushInline :: Pusher e Inline
pushInline = const pushnil  -- pushUD . typeInline
{-# INLINE pushInline #-}

-- | Retrieves an Inline value.
peekInline :: Peeker e Inline
peekInline = const (pure Space)  -- peekUD . typeInline
{-# INLINE peekInline #-}

-- | Retrieves a list of Inline values.
peekInlines :: LuaError e
            => Peeker e [Inline]
peekInlines = peekList peekInline
{-# INLINABLE peekInlines #-}

-- | Pushes a list of Inline values.
pushInlines :: LuaError e
            => Pusher e [Inline]
pushInlines = pushPandocList pushInline
{-# INLINABLE pushInlines #-}

-- | Try extra hard to retrieve an Inline value from the stack. Treats
-- bare strings as @Str@ values.
peekInlineFuzzy :: Peeker e Inline
peekInlineFuzzy idx = retrieving "Inline" $ liftLua (ltype idx) >>= \case
  TypeString   -> Str <$!> peekText idx
  _            -> peekInline idx
{-# INLINABLE peekInlineFuzzy #-}

-- | Try extra-hard to return the value at the given index as a list of
-- inlines.
peekInlinesFuzzy :: LuaError e
                 => Peeker e [Inline]
peekInlinesFuzzy idx = liftLua (ltype idx) >>= \case
  TypeString -> B.toList . B.text <$> peekText idx
  _ -> choice
       [ peekList peekInlineFuzzy
       , fmap pure . peekInlineFuzzy
       ] idx
{-# INLINABLE peekInlinesFuzzy #-}
