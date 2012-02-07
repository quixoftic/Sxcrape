{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

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

-- All x.id:xxx Redis keys should derive from this type class.
class IDKey a where
  idKeyToText :: a -> T.Text

-- All next.x.id Redis keys should derive from this type class.
class NextIDKey a where
  nextIDKeyToText :: a -> T.Text

-- All key types should wrap this newtype, rather than T.Text. It
-- prevents Haskell from automatically converting T.Text arguments to
-- IDKey or NextIDKey when applying getOrSetID.
--
newtype Key a = Key { getKey :: T.Text } deriving (Eq, Show)

instance IDKey (Key a) where
  idKeyToText = getKey

instance NextIDKey (Key a) where
  nextIDKeyToText = getKey

-- Build keys from namespaces with this operator.
(<:>) :: T.Text -> T.Text -> T.Text
s1 <:> s2 = T.concat [s1, ":", s2]

-- Event keys.
data Event
newtype EventIDKey = EventIDKey { getEventIDKey :: (Key Event) } deriving (Eq, Show, IDKey)
newtype NextEventIDKey = NextEventIDKey { getNextEventIDKey :: (Key Event) } deriving (Eq, Show, NextIDKey)

nextEventIDKey :: NextEventIDKey
nextEventIDKey = NextEventIDKey $ Key "next.event.id"

eventIDKeyPrefix :: T.Text
eventIDKeyPrefix = "event.id"

eventIDKey :: T.Text -> EventIDKey
eventIDKey nativeEventID = EventIDKey $ Key $ eventIDKeyPrefix <:> nativeEventID

getOrSetEventID :: T.Text -> Redis -> IO (Int)
getOrSetEventID nativeEventID r = getOrSetID (eventIDKey nativeEventID) nextEventIDKey r

-- artistIDKeyPrefix :: T.Text
-- artistIDKeyPrefix = "artist.id"

-- nextArtistIDKey :: T.Text
-- nextArtistIDKey = "next.artist.id"

-- artistIDKey :: T.Text -> T.Text
-- artistIDKey nativeArtistID = artistIDKeyPrefix <:> nativeArtistID

-- getOrSetArtistID :: T.Text -> Redis -> IO (Int)
-- getOrSetArtistID nativeArtistID r = getOrSetID (artistIDKey nativeArtistID) nextArtistIDKey r

-- venueIDKeyPrefix :: T.Text
-- venueIDKeyPrefix = "venue.id"

-- nextVenueIDKey :: T.Text
-- nextVenueIDKey = "next.venue.id"

-- venueIDKey :: T.Text -> T.Text
-- venueIDKey nativeVenueID = venueIDKeyPrefix <:> nativeVenueID

-- getOrSetVenueID :: T.Text -> Redis -> IO (Int)
-- getOrSetVenueID nativeVenueID r = getOrSetID (venueIDKey nativeVenueID) nextVenueIDKey r

-- Return an ID for idKey if it exists, otherwise make a new one by
-- incrementing the next ID key. This function is race-free. If two
-- processes call it at the same time, only one will create a new ID;
-- the other will return the same ID as the first.
--
getOrSetID :: (IDKey a, NextIDKey b) => a -> b -> Redis -> IO (Int)
getOrSetID idKey nextIdKey r = do
  id <- get r (idKeyToText idKey) >>= fromRBulk
  case id of
    Just theID -> return theID
    Nothing -> do
      newID <- incr r (nextIDKeyToText nextIdKey) >>= fromRInt
      reply <- setNx r (idKeyToText idKey) newID >>= fromRInt
      if (reply == 1)
        then return newID
        else liftM fromJust $ get r (idKeyToText idKey) >>= fromRBulk

main :: IO (Int)
main = connect localhost defaultPort >>= getOrSetEventID "MS1005"

-- Canary test for key type-safety. This should not compile:
--
-- getOrSetFooID :: T.Text -> T.Text -> Redis -> IO (Int)
-- getOrSetFooID x y r = getOrSetID x y r
