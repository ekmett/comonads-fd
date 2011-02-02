-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Store.Memo
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Store.Memo ( 
  -- * ComonadStore class
    ComonadStore(..)
  -- * The Store comonad
  , Store
  , store
  , runStore
  -- * The StoreT comonad transformer
  , StoreT
  , storeT
  , runStoreT
  -- * Re-exported modules
  , module Control.Comonad
  , module Control.Comonad.Trans.Class
  ) where

import Control.Comonad
import Control.Comonad.Store.Class (ComonadStore(..))
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Store.Memo (Store, store, runStore, StoreT, storeT, runStoreT)
