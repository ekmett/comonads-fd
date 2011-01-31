{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Discont.Class
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Discont.Class 
  ( ComonadDiscont(..)
  ) where

import Control.Comonad
import qualified Control.Comonad.Trans.Discont.Strict as Strict
import qualified Control.Comonad.Trans.Discont.Lazy as Lazy
import qualified Control.Comonad.Trans.Discont.Memo as Memo
{-
import Control.Comonad.Trans.Class
import qualified Control.Comonad.Trans.Env.Strict as Strict
import qualified Control.Comonad.Trans.Store.Strict as Strict
import qualified Control.Comonad.Trans.Env.Lazy as Lazy
import qualified Control.Comonad.Trans.Store.Lazy as Lazy
import qualified Control.Comonad.Trans.Traced as Simple
import qualified Control.Comonad.Trans.Traced.Memo as Memo
import qualified Control.Comonad.Trans.Store.Memo as Memo
import Control.Comonad.Trans.Stream
import Control.Comonad.Trans.Identity 
import Data.Monoid
import Data.Semigroup
-}

class Comonad w => ComonadDiscont s w | w -> s where
  callCV :: w (w (w a -> a) -> b) -> b

instance ComonadDiscont s w => ComonadDiscont s (Lazy.DiscontT k w) where
  callCV = Lazy.callCV

instance ComonadDiscont s w => ComonadDiscont s (Memo.DiscontT k w) where
  callCV = Memo.callCV

instance ComonadDiscont s w => ComonadDiscont s (Strict.DiscontT k w) where
  callCV = Strict.callCV

{-
instance Comonad w => ComonadDiscont s (Strict.StoreT s w) where

instance Comonad w => ComonadDiscont s (Lazy.StoreT s w) where

instance Comonad w => ComonadDiscont s (Memo.StoreT s w) where

instance ComonadDiscont s w => ComonadDiscont s (IdentityT w) where

instance ComonadDiscont s w => ComonadDiscont s (Lazy.EnvT e w) where

instance ComonadDiscont s w => ComonadDiscont s (Strict.EnvT e w) where

instance (ComonadDiscont s w, Semigroup m, Monoid m) => ComonadDiscont s (Simple.TracedT m w) where

instance (ComonadDiscont s w, Monoid m) => ComonadDiscont s (Memo.TracedT m w) where

instance (ComonadDiscont s w, Functor f) => ComonadDiscont s (StreamT f w) where
-}
