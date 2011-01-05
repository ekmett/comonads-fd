-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Context
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Context
  ( 
  -- * ComonadContext class
    ComonadContext(..)
  , gets
  , experiment
  -- * The Context comonad
  , Context
  , context
  , runContext
  -- * The ContextT comonad transformer
  , ContextT(..)
  , runContextT,
  -- * Re-exported modules
  module Control.Comonad,
  module Control.Comonad.Trans.Class
  ) where

import Control.Comonad
import Control.Comonad.Context.Class (ComonadContext(..), gets, experiment)
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Context (Context, context, runContext, ContextT(..), runContextT)
