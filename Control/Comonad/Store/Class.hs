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
  , lowerGet
  ) where

import Control.Comonad
import Control.Comonad.Trans.Class
import qualified Control.Comonad.Trans.Env.Strict as Strict
import qualified Control.Comonad.Trans.Store.Strict as Strict
import qualified Control.Comonad.Trans.Discont.Strict as Strict

import qualified Control.Comonad.Trans.Env.Lazy as Lazy
import qualified Control.Comonad.Trans.Store.Lazy as Lazy
import qualified Control.Comonad.Trans.Discont.Lazy as Lazy

import qualified Control.Comonad.Trans.Traced as Simple

import qualified Control.Comonad.Trans.Traced.Memo as Memo
import qualified Control.Comonad.Trans.Store.Memo as Memo
import qualified Control.Comonad.Trans.Discont.Memo as Memo
-- import Control.Comonad.Trans.Stream
import Control.Comonad.Trans.Identity 
import Data.Monoid
import Data.Semigroup

class Comonad w => ComonadStore s w | w -> s where
  get :: w a -> s
  put :: s -> w a -> w a
  modify :: (s -> s) -> w a -> w a
  modify f wa = put (f (get wa)) wa

gets :: ComonadStore s w => (s -> t) -> w a -> t
gets f wa = f (get wa)
{-# INLINE gets #-}

experiment :: (ComonadStore s w, Functor f) => f (s -> s) -> w a -> f a
experiment ff wa = fmap (\f -> extract (modify f wa)) ff
{-# INLINE experiment #-}

instance Comonad w => ComonadStore s (Strict.StoreT s w) where
  get = Strict.get
  put = Strict.put
  modify = Strict.modify

instance Comonad w => ComonadStore s (Lazy.StoreT s w) where
  get = Lazy.get
  put = Lazy.put
  modify = Lazy.modify

instance Comonad w => ComonadStore s (Memo.StoreT s w) where
  get = Memo.get
  put = Memo.put
  modify = Memo.modify

-- All of these require UndecidableInstances because they do not satisfy the coverage condition

lowerGet :: (ComonadTrans t, ComonadStore s w) => t w a -> s
lowerGet = get . lower
{-# INLINE lowerGet #-}

instance ComonadStore s w => ComonadStore s (Lazy.DiscontT k w) where
  get = lowerGet
  put s ~(Lazy.DiscontT f wa) = Lazy.DiscontT f (put s wa)

instance ComonadStore s w => ComonadStore s (Memo.DiscontT k w) where
  get = lowerGet
  put s w = Memo.discontT f (put s wa)
    where (f, wa) = Memo.runDiscontT w

instance ComonadStore s w => ComonadStore s (Strict.DiscontT k w) where
  get = lowerGet
  put s (Strict.DiscontT f wa) = Strict.DiscontT f (put s wa)

instance ComonadStore s w => ComonadStore s (IdentityT w) where
  get = lowerGet
  put s = IdentityT . put s . runIdentityT

instance ComonadStore s w => ComonadStore s (Lazy.EnvT e w) where
  get = lowerGet
  put s ~(Lazy.EnvT e wa) = Lazy.EnvT e (put s wa)

instance ComonadStore s w => ComonadStore s (Strict.EnvT e w) where
  get = lowerGet
  put s (Strict.EnvT e wa) = Strict.EnvT e (put s wa)

instance (ComonadStore s w, Semigroup m, Monoid m) => ComonadStore s (Simple.TracedT m w) where
  get = lowerGet
  put s (Simple.TracedT wma) = Simple.TracedT (put s wma)

instance (ComonadStore s w, Monoid m) => ComonadStore s (Memo.TracedT m w) where
  get = lowerGet
  put s = Memo.tracedT . put s . Memo.runTracedT
