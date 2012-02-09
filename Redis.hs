{-# LANGUAGE OverloadedStrings #-}

module Redis ( getOrSetEventID
             , getOrSetArtistID 
             , getOrSetVenueID
             , saddEvents
             , saddArtists
             , saddVenues
             ) where

import Database.Redis.Redis
import Database.Redis.ByteStringClass (BS, toBS, fromBS)
import Data.Maybe
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as E


getOrSetEventID :: T.Text -> Redis -> IO (Int)
getOrSetEventID nativeEventID = getOrSetID (eventIDKey nativeEventID) nextEventIDKey

getOrSetArtistID :: T.Text -> Redis -> IO (Int)
getOrSetArtistID nativeArtistID = getOrSetID (artistIDKey nativeArtistID) nextArtistIDKey

getOrSetVenueID :: T.Text -> Redis -> IO (Int)
getOrSetVenueID nativeVenueID = getOrSetID (venueIDKey nativeVenueID) nextVenueIDKey

saddEvents :: T.Text -> Redis -> IO (Int)
saddEvents nativeEventID = saddNativeID eventSetKey nativeEventID

saddArtists :: T.Text -> Redis -> IO (Int)
saddArtists nativeArtistID = saddNativeID artistSetKey nativeArtistID

saddVenues :: T.Text -> Redis -> IO (Int)
saddVenues nativeVenueID = saddNativeID venueSetKey nativeVenueID

instance BS T.Text where
  toBS = E.encodeUtf8
  fromBS = E.decodeUtf8

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
getOrSetID :: IDKey a Key -> NextIDKey a Key -> Redis -> IO (Int)
getOrSetID (IDKey _ idKey) (NextIDKey _ nextIDKey) r = do
  maybeID <- get r idKey >>= fromRBulk
  case maybeID of
    Just id -> return id
    Nothing -> do
      newID <- incr r nextIDKey >>= fromRInt
      reply <- setNx r idKey newID >>= fromRInt
      if (reply == 1)
        then return newID
        else liftM fromJust $ get r idKey >>= fromRBulk

-- Type-safe function for adding native (SXSW) IDs to a set.
--
saddNativeID :: SetKey a Key -> T.Text -> Redis -> IO (Int)
saddNativeID (SetKey _ key) nativeID r = do
  sadd r key nativeID >>= fromRInt

-- Canary test for key type-safety. This should not compile:
--
-- getOrSetFooID :: T.Text -> T.Text -> Redis -> IO (Int)
-- getOrSetFooID x y r = getOrSetID x y r
