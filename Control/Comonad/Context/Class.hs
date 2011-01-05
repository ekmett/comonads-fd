{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Context.Class
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Context.Class 
  ( ComonadContext(..)
  , gets
  , experiment
  ) where

import Control.Comonad
import Control.Comonad.Trans.Class
import qualified Control.Comonad.Trans.Context as T
import qualified Control.Comonad.Trans.Pointer as P
import Control.Comonad.Trans.Discont
import Control.Comonad.Trans.Identity 
import Data.Ix

class Comonad w => ComonadContext s w | w -> s where
  get :: w a -> s
  put :: s -> w a -> a
  modify :: (s -> s) -> w a -> a

gets :: ComonadContext s w => (s -> t) -> w a -> t
gets f wa = f (get wa)

experiment :: (ComonadContext s w, Functor f) => f (s -> s) -> w a -> f a
experiment ff wa = fmap (`modify` wa) ff

instance Comonad w => ComonadContext s (T.ContextT s w) where
  get = T.get
  put = T.put
  modify = T.modify

instance (Comonad w, Ix i) => ComonadContext i (P.PointerT i w) where
  get = P.get
  put = P.put
  modify = P.modify

-- All of these require UndecidableInstances because they do not satisfy the coverage condition

instance ComonadContext s w => ComonadContext s (DiscontT t w) where
  get = get . lower
  put s = put s . lower 
  modify f = modify f . lower

instance ComonadContext s w => ComonadContext s (IdentityT w) where
  get = get . lower
  put s = put s . lower
  modify f = modify f . lower

