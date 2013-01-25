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

module Redis ( importEventDetails
             , allEvents
             , allVenues
             , allArtists
             ) where

import qualified ParseEventDoc
import Event
import Artist
import Venue
import Database.Redis
import Data.Maybe
import Control.Monad
import qualified Data.Aeson as Aeson (encode, decode)
import qualified Data.Aeson.Types as Aeson (ToJSON, FromJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as E

toJSON' :: (Aeson.ToJSON a) => a -> BS.ByteString
toJSON' = BS.concat . BL.toChunks . Aeson.encode

fromJSON' :: (Aeson.FromJSON a) => BS.ByteString -> Maybe a
fromJSON' bs = Aeson.decode $ BL.fromChunks [bs]

-- Redis keys
--
(<:>) :: T.Text -> T.Text -> BS.ByteString
s1 <:> s2 = toBS $ T.concat [s1, ":", s2]

toEventKey :: T.Text -> BS.ByteString
toEventKey = (<:>) "event"

toArtistKey :: T.Text -> BS.ByteString
toArtistKey = (<:>) "artist"

toVenueKey :: T.Text -> BS.ByteString
toVenueKey = (<:>) "venue"

eventSetKey :: BS.ByteString
eventSetKey = "events"

artistSetKey :: BS.ByteString
artistSetKey = "artists"

venueSetKey :: BS.ByteString
venueSetKey = "venues"

-- Import functions.
--
importEventDetails :: (Event, Artist, Venue) -> Redis ()
importEventDetails (event, artist, venue) =
  let artistName = Artist.name artist
      venueName = Venue.name venue
      eventURL = Event.url event
  in do
    unsafeSet (toEventKey eventURL) (toJSON' event)
    unsafeSet (toArtistKey artistName) (toJSON' artist)
    unsafeSet (toVenueKey venueName) (toJSON' venue)
    unsafeSadd eventSetKey (toBS eventURL)
    unsafeSadd artistSetKey (toBS artistName)
    unsafeSadd venueSetKey (toBS venueName)
    return ()

-- Export functions.
--
allEvents :: Redis ([Event])
allEvents = allX eventSetKey toEventKey

allArtists :: Redis ([Artist])
allArtists = allX artistSetKey toArtistKey

allVenues :: Redis ([Venue])
allVenues = allX venueSetKey toVenueKey

allX :: Aeson.FromJSON a => BS.ByteString -> (T.Text -> BS.ByteString) -> Redis ([a])
allX setKey toKeyFn = do
  xs <- unsafeSmembers setKey
  maybeJSONs <- unsafeMget $ map toKeyFn $ map fromBS xs
  return $ catMaybes $ map fromJSON' $ catMaybes maybeJSONs

-- Convenience wrappers around Redis functions.
--
unsafeSet :: BS.ByteString -> BS.ByteString -> Redis (Status)
unsafeSet key value = do
  Right status <- set key value
  return status

unsafeSadd :: BS.ByteString -> BS.ByteString -> Redis (Integer)
unsafeSadd key value = do
  Right result <- sadd key [value]
  return result

unsafeMget :: [BS.ByteString] -> Redis ([Maybe BS.ByteString])
unsafeMget keys = do
  Right result <- mget keys
  return result

unsafeSmembers :: BS.ByteString -> Redis ([BS.ByteString])
unsafeSmembers key = do
  Right result <- smembers key
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
