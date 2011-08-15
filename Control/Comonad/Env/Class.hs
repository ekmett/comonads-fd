{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Env.Class
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Env.Class 
  ( ComonadEnv(..)
  , asks
  ) where

import Control.Comonad
import Control.Comonad.Trans.Class
import qualified Control.Comonad.Trans.Env.Lazy as Lazy
import qualified Control.Comonad.Trans.Store.Lazy as Lazy
import qualified Control.Comonad.Trans.Discont.Lazy as Lazy
import qualified Control.Comonad.Trans.Env.Strict as Strict
import qualified Control.Comonad.Trans.Store.Strict as Strict
import qualified Control.Comonad.Trans.Discont.Strict as Strict
import qualified Control.Comonad.Trans.Traced as Simple
import qualified Control.Comonad.Trans.Traced.Memo as Memo
import qualified Control.Comonad.Trans.Store.Memo as Memo
import qualified Control.Comonad.Trans.Discont.Memo as Memo
import Control.Comonad.Trans.Identity 
import Data.Semigroup
-- import Data.Ix

class Comonad w => ComonadEnv e w | w -> e where
  ask :: w a -> e

asks :: ComonadEnv e w => (e -> e') -> w a -> e'
asks f wa = f (ask wa)
{-# INLINE asks #-}

instance Comonad w => ComonadEnv e (Lazy.EnvT e w) where
  ask = Lazy.ask

instance Comonad w => ComonadEnv e (Strict.EnvT e w) where
  ask = Strict.ask

instance ComonadEnv e ((,)e) where
  ask = fst

lowerAsk :: (ComonadEnv e w, ComonadTrans t) => t w a -> e
lowerAsk = ask . lower
{-# INLINE lowerAsk #-}

-- All of these require UndecidableInstances because they do not satisfy the coverage condition

-- instance (ComonadEnv e w, Ix i) => ComonadEnv e (PointerT i w) where
--   ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (Strict.StoreT t w) where
  ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (Strict.DiscontT t w) where
  ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (Lazy.StoreT t w) where
  ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (Lazy.DiscontT t w) where
  ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (Memo.StoreT t w) where
  ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (Memo.DiscontT t w) where
  ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (IdentityT w) where
  ask = lowerAsk

instance (ComonadEnv e w, Semigroup m, Monoid m) => ComonadEnv e (Simple.TracedT m w) where
  ask = lowerAsk

instance (ComonadEnv e w, Monoid m) => ComonadEnv e (Memo.TracedT m w) where
  ask = lowerAsk

