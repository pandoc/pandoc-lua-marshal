{- |
Copyright               : © 2021 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'Alignment' values.
-}
module HsLua.Pandoc.Types.Alignment
  ( peekAlignment
  , pushAlignment
  ) where

import HsLua
import Text.Pandoc.Definition (Alignment)

-- | Retrieves a 'Alignment' value from a string.
peekAlignment :: Peeker e Alignment
peekAlignment = peekRead

-- | Pushes a 'Alignment' value as a string.
pushAlignment :: Pusher e Alignment
pushAlignment = pushString . show
