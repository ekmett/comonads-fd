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

class Comonad w => ComonadDiscont s w | w -> s where
  callCV :: w (w (w a -> a) -> b) -> b

instance ComonadDiscont s w => ComonadDiscont s (Lazy.DiscontT k w) where
  callCV = Lazy.callCV

instance ComonadDiscont s w => ComonadDiscont s (Memo.DiscontT k w) where
  callCV = Memo.callCV

instance ComonadDiscont s w => ComonadDiscont s (Strict.DiscontT k w) where
  callCV = Strict.callCV

