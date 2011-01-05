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
import qualified Control.Comonad.Trans.Env as T
import qualified Control.Comonad.Trans.Pointer as P
import Control.Comonad.Trans.Discont
import Control.Comonad.Trans.Identity 
import Data.Ix

class Comonad w => ComonadEnv e w | w -> e where
  ask :: w a -> e

asks :: ComonadEnv e w => (e -> e') -> w a -> e'
asks f wa = f (ask wa)
{-# INLINE asks #-}

instance Comonad w => ComonadEnv e (T.EnvT e w) where
  ask = T.ask

instance ComonadEnv e ((,)e) where
  ask = fst

lowerAsk :: (ComonadEnv e w, ComonadTrans t) :: t w a -> e
lowerAsk = ask . lower
{-# INLINE lowerAsk #-}

-- All of these require UndecidableInstances because they do not satisfy the coverage condition

instance (ComonadEnv e w, Ix i) => ComonadEnv e (PointerT i w) where
  ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (ContextT t w) where
  ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (DiscontT t w) where
  ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (IdentityT w) where
  ask = lowerAsk
