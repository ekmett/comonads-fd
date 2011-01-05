-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Env
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Env ( 
  -- * ComonadEnv class
    ComonadEnv(..)
  , asks
  -- * The Env comonad
  , Env
  , env
  , runEnv
  -- * The EnvT comonad transformer
  , EnvT(..)
  , runEnvT
  -- * Re-exported modules
  , module Control.Comonad
  , module Control.Comonad.Trans.Class
  ) where

import Control.Comonad
import Control.Comonad.Env.Class (ComonadEnv(..), asks)
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env (Env, env, runEnv, EnvT(..), runEnvT)
