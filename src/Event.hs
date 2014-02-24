{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
--
-- Module      : Event
-- Copyright   : Copyright © 2014, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- SXSW music event representation.
--

module Event (Event(..)) where

import Data.Data (Data, Typeable)
import Data.Time as Time
import qualified Data.Text.Lazy as T
import GHC.Generics
import Data.Aeson

-- Some fields are optional, and are represented either as Maybe a, or as
-- an empty list.
--
-- The day of the event (e.g., "2013-03-17") is recorded along with
-- the exact start and end times because some events don't have a
-- start and end time.
data Event = Event { url :: T.Text
                   , artist :: T.Text
                   , venue :: T.Text
                   , day :: T.Text
                   , start :: Maybe UTCTime
                   , end :: Maybe UTCTime
                   , ages :: Maybe T.Text
                   , hashTags :: [T.Text]
                   } deriving (Show, Data, Typeable, Generic)

instance ToJSON Event

instance FromJSON Event
