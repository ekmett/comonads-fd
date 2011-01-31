{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Traced.Class
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Traced.Class 
  ( ComonadTraced(..)
  , traces
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

class Comonad w => ComonadTraced m w | w -> m where
  trace :: m -> w a -> a

traces :: ComonadTraced m w => (a -> m) -> w a -> a
traces f wa = trace (f (extract wa)) wa
{-# INLINE traces #-}

instance (Comonad w, Semigroup m, Monoid m) => ComonadTraced m (Simple.TracedT m w) where
  trace = Simple.trace

instance (Comonad w, Monoid m) => ComonadTraced m (Memo.TracedT m w) where
  trace = Memo.trace

lowerTrace :: (ComonadTrans t, ComonadTraced m w) => m -> t w a -> a
lowerTrace m = trace m . lower
{-# INLINE lowerTrace #-}

-- All of these require UndecidableInstances because they do not satisfy the coverage condition

instance ComonadTraced m w => ComonadTraced m (IdentityT w) where
  trace = lowerTrace

instance ComonadTraced m w => ComonadTraced m (Strict.EnvT e w) where
  trace = lowerTrace

instance ComonadTraced m w => ComonadTraced m (Strict.DiscontT k w) where
  trace = lowerTrace

instance ComonadTraced m w => ComonadTraced m (Strict.StoreT s w) where
  trace = lowerTrace

instance ComonadTraced m w => ComonadTraced m (Lazy.EnvT e w) where
  trace = lowerTrace

instance ComonadTraced m w => ComonadTraced m (Lazy.DiscontT k w) where
  trace = lowerTrace

instance ComonadTraced m w => ComonadTraced m (Lazy.StoreT s w) where
  trace = lowerTrace

instance ComonadTraced m w => ComonadTraced m (Memo.DiscontT k w) where
  trace = lowerTrace

instance ComonadTraced m w => ComonadTraced m (Memo.StoreT s w) where
  trace = lowerTrace
