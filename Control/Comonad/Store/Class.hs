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
  , lowerPos
  , lowerPeek
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
import Control.Comonad.Trans.Identity 
import Data.Monoid
import Data.Semigroup

class Comonad w => ComonadStore s w | w -> s where
  pos :: w a -> s
  peek :: s -> w a -> a

  peeks :: (s -> s) -> w a -> a
  peeks f w = peek (f (pos w)) w
  
  seek :: s -> w a -> w a
  seek s = peek s . duplicate
    
  seeks :: (s -> s) -> w a -> w a
  seeks f = peeks f . duplicate

instance Comonad w => ComonadStore s (Strict.StoreT s w) where
  pos = Strict.pos
  peek = Strict.peek
  peeks = Strict.peeks
  seek = Strict.seek
  seeks = Strict.seeks

instance Comonad w => ComonadStore s (Lazy.StoreT s w) where
  pos = Lazy.pos
  peek = Lazy.peek
  peeks = Lazy.peeks
  seek = Lazy.seek
  seeks = Lazy.seeks

instance Comonad w => ComonadStore s (Memo.StoreT s w) where
  pos = Memo.pos
  peek = Memo.peek
  peeks = Memo.peeks
  seek = Memo.seek
  seeks = Memo.seeks

-- All of these require UndecidableInstances because they do not satisfy the coverage condition

lowerPos :: (ComonadTrans t, ComonadStore s w) => t w a -> s
lowerPos = pos . lower
{-# INLINE lowerPos #-}

lowerPeek :: (ComonadTrans t, ComonadStore s w) => s -> t w a -> a
lowerPeek s = peek s . lower
{-# INLINE lowerPeek #-}

instance ComonadStore s w => ComonadStore s (Lazy.DiscontT k w) where
  pos = lowerPos
  peek = lowerPeek

instance ComonadStore s w => ComonadStore s (Memo.DiscontT k w) where
  pos = lowerPos
  peek = lowerPeek

instance ComonadStore s w => ComonadStore s (Strict.DiscontT k w) where
  pos = lowerPos
  peek = lowerPeek

instance ComonadStore s w => ComonadStore s (IdentityT w) where
  pos = lowerPos
  peek = lowerPeek

instance ComonadStore s w => ComonadStore s (Lazy.EnvT e w) where
  pos = lowerPos
  peek = lowerPeek

instance ComonadStore s w => ComonadStore s (Strict.EnvT e w) where
  pos = lowerPos
  peek = lowerPeek

instance (ComonadStore s w, Semigroup m, Monoid m) => ComonadStore s (Simple.TracedT m w) where
  pos = lowerPos
  peek = lowerPeek

instance (ComonadStore s w, Monoid m) => ComonadStore s (Memo.TracedT m w) where
  pos = lowerPos
  peek = lowerPeek
