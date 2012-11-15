{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
--
-- Module      : Venue
-- Copyright   : Copyright © 2012, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- SXSW music venue representation.
--

module Venue (Venue(..)) where

import Data.Data (Data, Typeable)
import qualified Data.Text.Lazy as T
import GHC.Generics
import Data.Aeson

-- Some fields are optional, and are represented either as Maybe a, or as
-- an empty list.
data Venue = Venue { name :: T.Text
                   , address :: T.Text
                   } deriving (Show, Data, Typeable, Generic)

instance ToJSON Venue
