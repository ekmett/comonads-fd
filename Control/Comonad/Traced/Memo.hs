-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Traced.Memo
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Traced.Memo ( 
  -- * ComonadTraced class
    ComonadTraced(..)
  , traces
  -- * The Traced comonad
  , Traced
  , traced
  , runTraced
  -- * The TracedT comonad transformer
  , TracedT
  , tracedT
  , runTracedT
  -- * Re-exported modules
  , module Control.Comonad
  , module Control.Comonad.Trans.Class
  ) where

import Control.Comonad
import Control.Comonad.Traced.Class (ComonadTraced(..), traces)
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Traced.Memo (Traced, traced, runTraced, TracedT, tracedT, runTracedT)
