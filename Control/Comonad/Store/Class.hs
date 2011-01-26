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
import qualified Control.Comonad.Trans.Store.Strict as Strict
import qualified Control.Comonad.Trans.Store.Lazy as Lazy
import qualified Control.Comonad.Trans.Discont.Strict as Strict
import qualified Control.Comonad.Trans.Discont.Lazy as Lazy
import qualified Control.Comonad.Trans.Env.Strict as Strict
import qualified Control.Comonad.Trans.Env.Lazy as Lazy
import Control.Comonad.Trans.Stream
import Control.Comonad.Trans.Identity 
import Control.Comonad.Trans.Traced
import Data.Monoid
import Data.Semigroup
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

instance Comonad w => ComonadStore s (Strict.StoreT s w) where
  get = Strict.get
  put = Strict.put
  modify = Strict.modify

instance Comonad w => ComonadStore s (Lazy.StoreT s w) where
  get = Lazy.get
  put = Lazy.put
  modify = Lazy.modify

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

instance ComonadStore s w => ComonadStore s (Lazy.DiscontT k w) where
  get = lowerGet
  put = lowerPut
  modify = lowerModify

instance ComonadStore s w => ComonadStore s (Strict.DiscontT k w) where
  get = lowerGet
  put = lowerPut
  modify = lowerModify

instance ComonadStore s w => ComonadStore s (IdentityT w) where
  get = lowerGet
  put = lowerPut
  modify = lowerModify

instance ComonadStore s w => ComonadStore s (Lazy.EnvT e w) where
  get = lowerGet
  put = lowerPut
  modify = lowerModify

instance ComonadStore s w => ComonadStore s (Strict.EnvT e w) where
  get = lowerGet
  put = lowerPut
  modify = lowerModify

instance (ComonadStore s w, Semigroup m, Monoid m) => ComonadStore s (TracedT m w) where
  get = lowerGet
  put = lowerPut
  modify = lowerModify

instance (ComonadStore s w, Functor f) => ComonadStore s (StreamT f w) where
  get = lowerGet
  put = lowerPut
  modify = lowerModify
