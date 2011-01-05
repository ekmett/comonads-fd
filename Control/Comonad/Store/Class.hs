{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Store.Class
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Store.Class 
  ( ComonadStore(..)
  , gets
  , experiment
  ) where

import Control.Comonad
import Control.Comonad.Trans.Class
import qualified Control.Comonad.Trans.Store as T
import Control.Comonad.Trans.Discont
import Control.Comonad.Trans.Env
import Control.Comonad.Trans.Identity 
import Control.Comonad.Trans.Traced
import Data.Monoid
-- import qualified Control.Comonad.Trans.Pointer as P
-- import Data.Ix

class Comonad w => ComonadStore s w | w -> s where
  get :: w a -> s
  put :: s -> w a -> a
  modify :: (s -> s) -> w a -> a

  modify f wa = put (f (get wa)) wa

gets :: ComonadStore s w => (s -> t) -> w a -> t
gets f wa = f (get wa)
{-# INLINE gets #-}

experiment :: (ComonadStore s w, Functor f) => f (s -> s) -> w a -> f a
experiment ff wa = fmap (`modify` wa) ff
{-# INLINE experiment #-}

instance Comonad w => ComonadStore s (T.StoreT s w) where
  get = T.get
  put = T.put
  modify = T.modify

{-
instance (Comonad w, Ix i) => ComonadStore i (P.PointerT i w) where
  get = P.get
  put = P.put
  modify = P.modify
-}

-- All of these require UndecidableInstances because they do not satisfy the coverage condition

lowerGet :: (ComonadTrans t, ComonadStore s w) => t w a -> s
lowerGet = get . lower
{-# INLINE lowerGet #-}

lowerPut :: (ComonadTrans t, ComonadStore s w) => s -> t w a -> a
lowerPut s = put s . lower
{-# INLINE lowerPut #-}

lowerModify :: (ComonadTrans t, ComonadStore s w) => (s -> s) -> t w a -> a
lowerModify f = modify f . lower
{-# INLINE lowerModify #-}

instance ComonadStore s w => ComonadStore s (DiscontT k w) where
  get = lowerGet
  put = lowerPut
  modify = lowerModify

instance ComonadStore s w => ComonadStore s (IdentityT w) where
  get = lowerGet
  put = lowerPut
  modify = lowerModify

instance ComonadStore s w => ComonadStore s (EnvT e w) where
  get = lowerGet
  put = lowerPut
  modify = lowerModify

instance (ComonadStore s w, Monoid m) => ComonadStore s (TracedT m w) where
  get = lowerGet
  put = lowerPut
  modify = lowerModify
