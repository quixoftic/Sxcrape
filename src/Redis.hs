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

module Redis ( importEvent,
               getOrSetEventID
             , getOrSetArtistID 
             , getOrSetVenueID
             , saddEvents
             , saddArtists
             , saddVenues
             ) where

import qualified ParseEventDoc
import Event
import Database.Redis
import Data.Maybe
import Control.Monad
import qualified Data.Aeson.Encode as Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as E

-- Import the event and return the internal event ID.
importEvent :: Event -> Redis (BS.ByteString)
importEvent event =
  let artist = Event.artist event
      venue = Event.venue event
      url = Event.url event
  in do
    eventID <- getOrSetEventID url
    setEventJSON (eventJSONKey $ fromBS eventID) event
    saddEvents url
    saddArtists artist
    saddVenues venue
    return eventID
  
getOrSetEventID :: T.Text -> Redis (BS.ByteString)
getOrSetEventID nativeEventID = getOrSetID (eventIDKey nativeEventID) nextEventIDKey

getOrSetArtistID :: T.Text -> Redis (BS.ByteString)
getOrSetArtistID nativeArtistID = getOrSetID (artistIDKey nativeArtistID) nextArtistIDKey

getOrSetVenueID :: T.Text -> Redis (BS.ByteString)
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
data JSONKey a k = JSONKey a k deriving (Eq, Show)

-- Build keys from namespaces with this operator.
(<:>) :: T.Text -> T.Text -> T.Text
s1 <:> s2 = T.concat [s1, ":", s2]

-- Event keys.
--
data EventT = EventT deriving (Eq, Show)
type EventIDKey = IDKey EventT Key
type NextEventIDKey = NextIDKey EventT Key
type EventSetKey = SetKey EventT Key
type EventJSONKey = JSONKey EventT Key

nextEventIDKey :: NextEventIDKey
nextEventIDKey = NextIDKey EventT "next.event.id"

eventIDKeyPrefix :: T.Text
eventIDKeyPrefix = "event.id"

eventIDKey :: T.Text -> EventIDKey
eventIDKey nativeEventID = IDKey EventT $ eventIDKeyPrefix <:> nativeEventID

eventSetKey :: EventSetKey
eventSetKey = SetKey EventT "events"

eventJSONKeyPrefix :: T.Text
eventJSONKeyPrefix = "event"

eventJSONKey :: T.Text -> EventJSONKey
eventJSONKey nativeEventID = JSONKey EventT $ eventJSONKeyPrefix <:> nativeEventID

-- Artist keys.
--
data ArtistT = ArtistT deriving (Eq, Show)
type ArtistIDKey = IDKey ArtistT Key
type NextArtistIDKey = NextIDKey ArtistT Key
type ArtistSetKey = SetKey ArtistT Key

artistIDKeyPrefix :: T.Text
artistIDKeyPrefix = "artist.id"

nextArtistIDKey :: NextArtistIDKey
nextArtistIDKey = NextIDKey ArtistT "next.artist.id"

artistIDKey :: T.Text -> ArtistIDKey
artistIDKey nativeArtistID = IDKey ArtistT  $ artistIDKeyPrefix <:> nativeArtistID

artistSetKey :: ArtistSetKey
artistSetKey = SetKey ArtistT "artists"

-- Venue keys.
--
data VenueT = VenueT deriving (Eq, Show)
type VenueIDKey = IDKey VenueT Key
type NextVenueIDKey = NextIDKey VenueT Key
type VenueSetKey = SetKey VenueT Key

venueIDKeyPrefix :: T.Text
venueIDKeyPrefix = "venue.id"

nextVenueIDKey :: NextVenueIDKey
nextVenueIDKey = NextIDKey VenueT "next.venue.id"

venueIDKey :: T.Text -> VenueIDKey
venueIDKey nativeVenueID = IDKey VenueT $ venueIDKeyPrefix <:> nativeVenueID

venueSetKey :: VenueSetKey
venueSetKey = SetKey VenueT "venues"


-- Generic functions.
--

-- Return an ID for idKey if it exists, otherwise make a new one by
-- incrementing the next ID key. This function is race-free. If two
-- processes call it at the same time, only one will create a new ID;
-- the other will return the same ID as the first.
--
getOrSetID :: IDKey a Key -> NextIDKey a Key -> Redis (BS.ByteString)
getOrSetID (IDKey _ idKey) (NextIDKey _ nextIDKey) =
  let key = toBS idKey
      nextkey = toBS nextIDKey
    in do
    Right maybeID <- get key
    case maybeID of
      Just id -> return id
      Nothing -> do
        Right newID <- incr nextkey
        Right reply <- setnx key $ integerToValue newID
        if reply
          then return $ integerToValue newID
          else do Right (Just id) <- get key
                  return id

setEventJSON :: EventJSONKey -> Event -> Redis (Status)
setEventJSON (JSONKey _ eventKey) event =
  let key = toBS eventKey
      value = BS.concat $ BL.toChunks $ Aeson.encode event
  in do
    Right status <- set key value
    return status

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
--
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
--getOrSetFooID :: T.Text -> T.Text -> Redis (BS.ByteString)
--getOrSetFooID x y = getOrSetID x y
