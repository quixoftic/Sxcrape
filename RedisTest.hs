{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances #-}

import Database.Redis.Redis
import Database.Redis.ByteStringClass (BS, toBS, fromBS)
import Data.Maybe
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

-- Generic BS T.Text instance for converting all the *Key types to
-- Redis BS (as long as they derive from BS).
--
instance BS T.Text where
  toBS = E.encodeUtf8
  fromBS = E.decodeUtf8

data IDKey a k = IDKey a k deriving (Eq, Show)

idKey :: (IDKey a T.Text) -> T.Text
idKey (IDKey t k) = k

data NextIDKey a k = NextIDKey a k deriving  (Eq, Show)

nextIDKey :: (NextIDKey a T.Text) -> T.Text
nextIDKey (NextIDKey t k) = k

-- Build keys from namespaces with this operator.
(<:>) :: T.Text -> T.Text -> T.Text
s1 <:> s2 = T.concat [s1, ":", s2]

-- Event keys.
--
data Event = Event deriving (Eq, Show)
type EventIDKey = IDKey Event T.Text
type NextEventIDKey = NextIDKey Event T.Text

nextEventIDKey :: NextEventIDKey
nextEventIDKey = NextIDKey Event "next.event.id"

eventIDKeyPrefix :: T.Text
eventIDKeyPrefix = "event.id"

eventIDKey :: T.Text -> EventIDKey
eventIDKey nativeEventID = IDKey Event $ eventIDKeyPrefix <:> nativeEventID

getOrSetEventID :: T.Text -> Redis -> IO (Int)
getOrSetEventID nativeEventID r = getOrSetID (eventIDKey nativeEventID) nextEventIDKey r

-- Artist keys.
--
data Artist = Artist deriving (Eq, Show)
type ArtistIDKey = IDKey Artist T.Text
type NextArtistIDKey = NextIDKey Artist T.Text

artistIDKeyPrefix :: T.Text
artistIDKeyPrefix = "artist.id"

nextArtistIDKey :: NextArtistIDKey
nextArtistIDKey = NextIDKey Artist "next.artist.id"

artistIDKey :: T.Text -> ArtistIDKey
artistIDKey nativeArtistID = IDKey Artist $ artistIDKeyPrefix <:> nativeArtistID

getOrSetArtistID :: T.Text -> Redis -> IO (Int)
getOrSetArtistID nativeArtistID r = getOrSetID (artistIDKey nativeArtistID) nextArtistIDKey r

-- Venue keys.
--
data Venue = Venue deriving (Eq, Show)
type VenueIDKey = IDKey Venue T.Text
type NextVenueIDKey = NextIDKey Venue T.Text

venueIDKeyPrefix :: T.Text
venueIDKeyPrefix = "venue.id"

nextVenueIDKey :: NextVenueIDKey
nextVenueIDKey = NextIDKey Venue "next.venue.id"

venueIDKey :: T.Text -> VenueIDKey
venueIDKey nativeVenueID = IDKey Venue $ venueIDKeyPrefix <:> nativeVenueID

getOrSetVenueID :: T.Text -> Redis -> IO (Int)
getOrSetVenueID nativeVenueID r = getOrSetID (venueIDKey nativeVenueID) nextVenueIDKey r

-- Return an ID for idKey if it exists, otherwise make a new one by
-- incrementing the next ID key. This function is race-free. If two
-- processes call it at the same time, only one will create a new ID;
-- the other will return the same ID as the first.
--
getOrSetID :: IDKey a T.Text -> NextIDKey a T.Text -> Redis -> IO (Int)
getOrSetID theIDKey theNextIDKey r = do
  maybeID <- get r (idKey theIDKey) >>= fromRBulk
  case maybeID of
    Just id -> return id
    Nothing -> do
      newID <- incr r (nextIDKey theNextIDKey) >>= fromRInt
      reply <- setNx r (idKey theIDKey) newID >>= fromRInt
      if (reply == 1)
        then return newID
        else liftM fromJust $ get r (idKey theIDKey) >>= fromRBulk

main :: IO (Int)
main = connect localhost defaultPort >>= getOrSetEventID "MS1005"

-- Canary test for key type-safety. This should not compile:
--
-- getOrSetFooID :: T.Text -> T.Text -> Redis -> IO (Int)
-- getOrSetFooID x y r = getOrSetID x y r
