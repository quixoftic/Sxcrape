{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
--
-- Module      : Artist
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- SXSW music artist representation.
--

module Artist (Artist(..)) where

import Data.Data (Data, Typeable)
import qualified Data.Text.Lazy as T
import GHC.Generics
import Data.Aeson

-- Some fields are optional, and are represented either as Maybe a, or as
-- an empty list.
data Artist = Artist { name :: T.Text
                     , url :: Maybe T.Text
                     , genre :: Maybe T.Text
                     , origin :: Maybe T.Text
                     , imgURL :: T.Text
                     , songURL :: Maybe T.Text
                     , videoURL :: Maybe T.Text
                     } deriving (Show, Data, Typeable, Generic)

instance ToJSON Artist

instance FromJSON Artist
