{-# LANGUAGE OverloadedStrings #-}

import Database.Redis.Redis
import Database.Redis.ByteStringClass (BS, toBS, fromBS)
import Data.Maybe
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

instance BS T.Text where
  toBS = E.encodeUtf8
  fromBS = E.decodeUtf8

(<:>) :: T.Text -> T.Text -> T.Text
s1 <:> s2 = T.concat [s1, ":", s2]

eventIDKeyPrefix :: T.Text
eventIDKeyPrefix = "event.id"

nextEventIDKey :: T.Text
nextEventIDKey = "next.event.id"

eventIDKey :: T.Text -> T.Text
eventIDKey nativeEventID = eventIDKeyPrefix <:> nativeEventID

getOrSetEventID :: T.Text -> Redis -> IO (Int)
getOrSetEventID nativeEventID r = getOrSetID (eventIDKey nativeEventID) nextEventIDKey r

artistIDKeyPrefix :: T.Text
artistIDKeyPrefix = "artist.id"

nextArtistIDKey :: T.Text
nextArtistIDKey = "next.artist.id"

artistIDKey :: T.Text -> T.Text
artistIDKey nativeArtistID = artistIDKeyPrefix <:> nativeArtistID

getOrSetArtistID :: T.Text -> Redis -> IO (Int)
getOrSetArtistID nativeArtistID r = getOrSetID (artistIDKey nativeArtistID) nextArtistIDKey r

venueIDKeyPrefix :: T.Text
venueIDKeyPrefix = "venue.id"

nextVenueIDKey :: T.Text
nextVenueIDKey = "next.venue.id"

venueIDKey :: T.Text -> T.Text
venueIDKey nativeVenueID = venueIDKeyPrefix <:> nativeVenueID

getOrSetVenueID :: T.Text -> Redis -> IO (Int)
getOrSetVenueID nativeVenueID r = getOrSetID (venueIDKey nativeVenueID) nextVenueIDKey r

-- Return an ID for idKey if it exists, otherwise make a new one. This
-- function is race-free. If two processes call it at the same time,
-- only one will create a new ID; the other will return the same ID as
-- the first.
getOrSetID :: T.Text -> T.Text -> Redis ->IO (Int)
getOrSetID idKey nextIdKey r = do
  id <- get r idKey >>= fromRBulk
  case id of
    Just theID -> return theID
    Nothing -> do
      newID <- incr r nextIdKey >>= fromRInt
      reply <- setNx r idKey newID >>= fromRInt
      if (reply == 1)
        then return newID
        else liftM fromJust $ get r idKey >>= fromRBulk

insertKey :: Redis -> IO (Reply Int)
insertKey r = incr r ("test:key" :: T.Text)

main :: IO (Int)
main = connect localhost defaultPort >>= getOrSetEventID "MS1002"
