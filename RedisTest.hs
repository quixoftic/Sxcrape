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

eventID :: T.Text
eventID = "event.id"

nextEventID :: T.Text
nextEventID = "next.event.id"

newEventID :: Redis -> IO (Int)
newEventID r = incr r nextEventID >>= fromRInt
    
eventIDKey :: T.Text -> T.Text
eventIDKey nativeEventID = eventID <:> nativeEventID

getEventID :: T.Text -> Redis -> IO (Maybe Int)
getEventID nativeEventID r = get r (eventIDKey nativeEventID) >>= fromRBulk

setNxEventID :: T.Text -> Int -> Redis -> IO (Int)
setNxEventID nativeEventID id r = setNx r (eventIDKey nativeEventID) id >>= fromRInt

-- Return the event ID if it exists, otherwise make a new one. This
-- function is race-free. If two processes call it at the same time,
-- only one will create a new event ID; the other will return the same
-- event ID as the first.
getOrSetEventID :: T.Text -> Redis -> IO (Int)
getOrSetEventID nativeEventID r = do
  id <- getEventID nativeEventID r
  case id of
    Just theID -> return theID
    Nothing -> do
      newID <- newEventID r
      reply <- setNxEventID nativeEventID newID r
      if (reply == 1)
        then return newID
        else liftM fromJust $ getEventID nativeEventID r 

insertKey :: Redis -> IO (Reply Int)
insertKey r = incr r ("test:key" :: T.Text)

main :: IO (Int)
main = connect localhost defaultPort >>= getOrSetEventID "MS1001"
