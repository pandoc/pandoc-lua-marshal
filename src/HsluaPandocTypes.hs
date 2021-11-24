{- |
Copyright: (c) 2021 Albert Krewinkel
SPDX-License-Identifier: MIT
Maintainer: Albert Krewinkel <albert@zeitkraut.de>

Use pandoc types in Lua
-}

module HsluaPandocTypes
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
