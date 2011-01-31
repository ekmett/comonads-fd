-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Discont.Strict
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Discont.Strict ( 
  -- * ComonadDiscont class
    ComonadDiscont(..)
  , label
  -- * The Discont comonad
  , Discont
  , discont
  , runDiscont
  -- * The DiscontT comonad transformer
  , DiscontT(..)
  , runDiscontT
  -- * Re-exported modules
  , module Control.Comonad
  , module Control.Comonad.Trans.Class
  ) where

import Control.Comonad
import Control.Comonad.Discont.Class (ComonadDiscont(..))
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Discont.Strict (Discont, discont, runDiscont, DiscontT(..), runDiscontT, label)
