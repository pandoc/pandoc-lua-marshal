{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'MathType' values.
-}
module HsLua.Pandoc.Types.MathType
  ( peekMathType
  , pushMathType
  ) where

import HsLua
import Text.Pandoc.Definition (MathType)

-- | Retrieves a 'MathType' value from a string.
peekMathType :: Peeker e MathType
peekMathType = peekRead

-- | Pushes a 'MathType' value as a string.
pushMathType :: Pusher e MathType
pushMathType = pushString . show
