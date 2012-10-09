{-# LANGUAGE OverloadedStrings #-}
--
-- Module      : Redis
-- Copyright   : Copyright © 2012, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Experimental Redis interface for parsed SXSW music event info.
--

module Redis ( getOrSetEventID
             , getOrSetArtistID 
             , getOrSetVenueID
             , saddEvents
             , saddArtists
             , saddVenues
             ) where

import Database.Redis
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as E


getOrSetEventID :: T.Text -> Redis (Integer)
getOrSetEventID nativeEventID = getOrSetID (eventIDKey nativeEventID) nextEventIDKey

getOrSetArtistID :: T.Text -> Redis (Integer)
getOrSetArtistID nativeArtistID = getOrSetID (artistIDKey nativeArtistID) nextArtistIDKey

getOrSetVenueID :: T.Text -> Redis (Integer)
getOrSetVenueID nativeVenueID = getOrSetID (venueIDKey nativeVenueID) nextVenueIDKey

saddEvents :: T.Text -> Redis (Integer)
saddEvents nativeEventID = saddNativeID eventSetKey nativeEventID

saddArtists :: T.Text -> Redis (Integer)
saddArtists nativeArtistID = saddNativeID artistSetKey nativeArtistID

saddVenues :: T.Text -> Redis (Integer)
saddVenues nativeVenueID = saddNativeID venueSetKey nativeVenueID

type Key = T.Text

data IDKey a k = IDKey a k deriving (Eq, Show)
data NextIDKey a k = NextIDKey a k deriving (Eq, Show)
data SetKey a k = SetKey a k deriving (Eq, Show)

-- Build keys from namespaces with this operator.
(<:>) :: T.Text -> T.Text -> T.Text
s1 <:> s2 = T.concat [s1, ":", s2]

-- Event keys.
--
data Event = Event deriving (Eq, Show)
type EventIDKey = IDKey Event Key
type NextEventIDKey = NextIDKey Event Key
type EventSetKey = SetKey Event Key

nextEventIDKey :: NextEventIDKey
nextEventIDKey = NextIDKey Event "next.event.id"

eventIDKeyPrefix :: T.Text
eventIDKeyPrefix = "event.id"

eventIDKey :: T.Text -> EventIDKey
eventIDKey nativeEventID = IDKey Event $ eventIDKeyPrefix <:> nativeEventID

eventSetKey :: EventSetKey
eventSetKey = SetKey Event "events"

-- Artist keys.
--
data Artist = Artist deriving (Eq, Show)
type ArtistIDKey = IDKey Artist Key
type NextArtistIDKey = NextIDKey Artist Key
type ArtistSetKey = SetKey Artist Key

artistIDKeyPrefix :: T.Text
artistIDKeyPrefix = "artist.id"

nextArtistIDKey :: NextArtistIDKey
nextArtistIDKey = NextIDKey Artist "next.artist.id"

artistIDKey :: T.Text -> ArtistIDKey
artistIDKey nativeArtistID = IDKey Artist  $ artistIDKeyPrefix <:> nativeArtistID

artistSetKey :: ArtistSetKey
artistSetKey = SetKey Artist "artists"

-- Venue keys.
--
data Venue = Venue deriving (Eq, Show)
type VenueIDKey = IDKey Venue Key
type NextVenueIDKey = NextIDKey Venue Key
type VenueSetKey = SetKey Venue Key

venueIDKeyPrefix :: T.Text
venueIDKeyPrefix = "venue.id"

nextVenueIDKey :: NextVenueIDKey
nextVenueIDKey = NextIDKey Venue "next.venue.id"

venueIDKey :: T.Text -> VenueIDKey
venueIDKey nativeVenueID = IDKey Venue $ venueIDKeyPrefix <:> nativeVenueID

venueSetKey :: VenueSetKey
venueSetKey = SetKey Venue "venues"


-- Generic functions.
--

-- Return an ID for idKey if it exists, otherwise make a new one by
-- incrementing the next ID key. This function is race-free. If two
-- processes call it at the same time, only one will create a new ID;
-- the other will return the same ID as the first.
--
getOrSetID :: IDKey a Key -> NextIDKey a Key -> Redis (Integer)
getOrSetID (IDKey _ idKey) (NextIDKey _ nextIDKey) =
  let key = toBS idKey
      nextkey = toBS nextIDKey
    in do
    Right maybeID <- get key
    case maybeID of
      Just id -> return $ valueToInteger id
      Nothing -> do
        Right newID <- incr nextkey
        Right reply <- setnx key $ integerToValue newID
        if reply
          then return newID
          else do Right (Just id) <- get key
                  return $ valueToInteger id

  -- maybeID <- get r idKey >>= fromRBulk
  -- case maybeID of
  --   Just id -> return id
  --   Nothing -> do
  --     newID <- incr r nextIDKey >>= fromRInt
  --     reply <- setNx r idKey newID >>= fromRInt
  --     if (reply == 1)
  --       then return newID
  --       else liftM fromJust $ get r idKey >>= fromRBulk

-- Type-safe function for adding native (SXSW) IDs to a set.
--
saddNativeID :: SetKey a Key -> T.Text -> Redis (Integer)
saddNativeID (SetKey _ key) nativeID = 
  let k = toBS key
      id = toBS nativeID
  in do
    Right result <- sadd k [id]
    return result

-- Helpers

valueToInteger :: BS.ByteString -> Integer
valueToInteger = fst . fromJust . BS.readInteger

integerToValue :: Integer -> BS.ByteString
integerToValue = BS.pack . show

toBS :: T.Text -> BS.ByteString
toBS = E.encodeUtf8 . T.toStrict

fromBS :: BS.ByteString -> T.Text
fromBS = T.fromStrict . E.decodeUtf8


-- Canary test for key type-safety. This should not compile:
--
-- getOrSetFooID :: T.Text -> T.Text -> Redis -> IO (Int)
-- getOrSetFooID x y r = getOrSetID x y r
