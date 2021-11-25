{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{- |

Marshal values of types that make up 'Block' elements.
-}
module HsLua.Pandoc.Types.Block
  ( -- * Single Block elements
    peekBlock
  , peekBlockFuzzy
  , pushBlock
    -- * List of Blocks
  , peekBlocks
  , peekBlocksFuzzy
  , pushBlocks
  ) where

import Control.Monad ((<$!>))
import HsLua
import HsLua.Pandoc.Types.Inline (peekInlinesFuzzy)
import HsLua.Pandoc.Types.List (pushPandocList)
import Text.Pandoc.Definition

-- | Pushes an Block value as userdata object.
pushBlock :: Pusher e Block
pushBlock = const pushnil  -- pushUD . typeBlock
{-# INLINE pushBlock #-}

-- | Retrieves an Block value.
peekBlock :: Peeker e Block
peekBlock = const (pure Null)  -- peekUD . typeBlock
{-# INLINE peekBlock #-}

-- | Retrieves a list of Block values.
peekBlocks :: LuaError e
           => Peeker e [Block]
peekBlocks = peekList peekBlock
{-# INLINABLE peekBlocks #-}

-- | Pushes a list of Block values.
pushBlocks :: LuaError e
           => Pusher e [Block]
pushBlocks = pushPandocList pushBlock
{-# INLINABLE pushBlocks #-}

-- | Try extra hard to retrieve an Block value from the stack. Treats
-- bare strings as @Str@ values.
peekBlockFuzzy :: LuaError e
               => Peeker e Block
peekBlockFuzzy = choice
  [ peekBlock
  , (\idx -> Plain <$!> peekInlinesFuzzy idx)
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
