{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{- |
Module      : Text.Pandoc.Lua.Walk
Copyright   : © 2012-2021 John MacFarlane,
              © 2017-2021 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Walking documents in a filter-suitable way.
-}
module Text.Pandoc.Lua.Walk
  ( SpliceList (..)
  , Walkable
  , filterFunctionNames
  , walkSplice
  , walkElement
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Data ( Data, dataTypeConstrs, dataTypeName, dataTypeOf
                 , showConstr, toConstr, tyconUQname)
import Data.Proxy (Proxy (..))
import Data.String (IsString (fromString))
import HsLua
import Text.Pandoc.Lua.Marshal.Filter (Filter (..), pushFilterFunction)
import Text.Pandoc.Lua.SpliceList (SpliceList (..))
import Text.Pandoc.Walk
import qualified Data.Map.Strict as Map

-- | Walks an element, using a Lua 'Filter' to modify all values of type
-- @a@ that are in a list. The result of the called filter function must
-- be a retrieved as a list, and it is spliced back into the list at the
-- position of the original element. This allows to delete an element,
-- or to replace an element with multiple elements.
walkSplice :: forall e a b. (LuaError e, Data a, Walkable (SpliceList a) b)
           => Pusher e a
           -> Peeker e [a]
           -> Filter
           -> b -> LuaE e b
walkSplice pushElement peekElementOrList (Filter fnMap) =
  if any (`Map.member` fnMap) acceptedNames
  then walkM f
  else pure
 where
  f :: SpliceList a -> LuaE e (SpliceList a)
  f (SpliceList xs) = SpliceList <$> mconcatMapM tryFilter xs

  acceptedNames :: [Name]
  acceptedNames = catchAllName : filterFunctionNames (Proxy :: Proxy a)

  filterFnName :: a -> Name
  filterFnName = fromString . showConstr . toConstr

  catchAllName :: Name
  catchAllName = fromString . tyconUQname . dataTypeName . dataTypeOf $
                 (undefined :: a)

  tryFilter :: a -> LuaE e [a]
  tryFilter x =
    case filterFnName x `Map.lookup` fnMap <|>
         catchAllName   `Map.lookup` fnMap of
      Nothing -> pure [x]
      Just fn -> do
        pushFilterFunction fn
        pushElement x
        callWithTraceback 1 1
        forcePeek . (`lastly` pop 1) $ liftLua (ltype top) >>= \case
          TypeNil -> pure [x]  -- function returned `nil`, keep original value
          _       -> peekElementOrList top

mconcatMapM :: (Monad m) => (a -> m [a]) -> [a] -> m [a]
mconcatMapM f = fmap mconcat . mapM f

-- | Walks an element, modifying all values of type @a@ by applying the
-- given Lua 'Filter'.
walkElement :: forall e a b. (LuaError e, Walkable a b)
            => Name        -- ^ Name under which the filter function is stored
            -> Pusher e a
            -> Peeker e a
            -> Filter
            -> b -> LuaE e b
walkElement filterFnName pushElement peekElement (Filter fnMap) =
  if filterFnName `Map.member` fnMap
  then walkM f
  else return
 where
  f :: a -> LuaE e a
  f x = case Map.lookup filterFnName fnMap of
          Just fn -> do
            pushFilterFunction fn
            pushElement x
            callWithTraceback 1 1
            forcePeek . (`lastly` pop 1) $
              (x <$ peekNil top) <|> peekElement top
          Nothing -> pure x

-- | Filter function names for a given type.
filterFunctionNames :: forall a. Data a => Proxy a -> [Name]
filterFunctionNames _ =
  map (fromString . show) . dataTypeConstrs $ dataTypeOf (undefined :: a)

-- | Like @'Lua.call'@, but adds a traceback to the error message (if any).
callWithTraceback :: forall e. LuaError e
                  => NumArgs -> NumResults -> LuaE e ()
callWithTraceback nargs nresults = do
  let traceback' :: LuaE e NumResults
      traceback' = do
        l <- state
        msg <- tostring' (nthBottom 1)
        traceback l (Just msg) 2
        return 1
  tracebackIdx <- absindex (nth (fromNumArgs nargs + 1))
  pushHaskellFunction traceback'
  insert tracebackIdx
  result <- pcall nargs nresults (Just tracebackIdx)
  remove tracebackIdx
  when (result /= OK)
    throwErrorAsException
