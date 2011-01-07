-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Store.Strict
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Store.Strict ( 
  -- * ComonadStore class
    ComonadStore(..)
  , gets
  , experiment
  -- * The Store comonad
  , Store
  , store
  , runStore
  -- * The StoreT comonad transformer
  , StoreT(..)
  , runStoreT
  -- * Re-exported modules
  , module Control.Comonad
  , module Control.Comonad.Trans.Class
  ) where

import Control.Comonad
import Control.Comonad.Store.Class (ComonadStore(..), gets, experiment)
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Store.Strict (Store, store, runStore, StoreT(..), runStoreT)
