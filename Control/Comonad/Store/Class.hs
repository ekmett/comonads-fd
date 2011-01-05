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
import Control.Comonad.Trans.Identity 
-- import qualified Control.Comonad.Trans.Pointer as P
-- import Data.Ix

class Comonad w => ComonadStore s w | w -> s where
  get :: w a -> s
  put :: s -> w a -> a
  modify :: (s -> s) -> w a -> a

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

instance ComonadStore s w => ComonadStore s (DiscontT t w) where
  get = get . lower
  put s = put s . lower 
  modify f = modify f . lower

instance ComonadStore s w => ComonadStore s (IdentityT w) where
  get = get . lower
  put s = put s . lower
  modify f = modify f . lower
