{-# LANGUAGE DeriveDataTypeable #-}

module Event ( Event
             , parseEvent
             ) where

import Data.Maybe
import Control.Monad
import Data.String.Utils as String
import Data.Time as Time
import Locale as Locale
import Text.XML.Light
import ParserUtils
import Data.Data (Data, Typeable)

-- It's entirely possible that some events will be missing one or more
-- of these details. Those that are non-essential (e.g., artistURL)
-- might need to be changed to type Maybe String. Let's see how it
-- goes.

data Event = Event { artist :: String
                   , venue :: String
                   , address :: String
                   , start :: UTCTime
                   , ages :: String
                   , genre :: String
                   , description :: String
                   , artistURL :: String
                   , origin :: String
                   , imgURL :: String
                   } deriving (Show, Data, Typeable)
                     
-- Might need another variant of parseEvent that returns an Either
-- String Event, or throws an exception, to provide details about why
-- a parse failed. For now, this will do.

parseEvent :: Element -> Maybe Event
parseEvent xml = do
  theArtist <- parseArtist xml
  theVenue <- parseVenue xml
  theAddress <- parseAddress xml
  theStart <- parseStart xml
  theAges <- parseAges xml
  theGenre <- parseGenre xml
  theDescription <- parseDescription xml
  theArtistURL <- parseArtistURL xml
  theOrigin <- parseOrigin xml
  theImgURL <- parseImgURL xml
  return Event { artist = theArtist
               , venue = theVenue
               , address = theAddress
               , start = theStart
               , ages = theAges
               , genre = theGenre
               , description = theDescription
               , artistURL = theArtistURL
               , origin = theOrigin
               , imgURL = theImgURL
               }
                 
-- origin often has weird formatting.
parseOrigin :: Element -> Maybe String
parseOrigin = fmap (String.join ", " . words) . maybeStrContent . findClass "event_citystate"

-- Strip out the description line formatting.
parseDescription :: Element -> Maybe String
parseDescription = fmap (String.replace "\n" "") . maybeStrContent . findClass "main_content_desc"

parseArtist :: Element -> Maybe String
parseArtist = maybeStrContent . findClass "event_name"

parseGenre :: Element -> Maybe String
parseGenre =  maybeStrContent . findClass "event_sub_category"

parseImgURL :: Element -> Maybe String
parseImgURL = theSrc <=< findImg <=< findClass "video_embed"

dateStr :: Element -> Maybe String
dateStr = maybeStrContent . findClass "date"

timeStr :: Element -> Maybe String
timeStr = maybeStrContent . findClass "time"

parseVenue :: Element -> Maybe String
parseVenue = maybeStrContent . findLink <=< listToMaybe . findClasses "venue"

parseAddress :: Element -> Maybe String
parseAddress = maybeStrContent . findClass "address"

parseArtistURL :: Element -> Maybe String
parseArtistURL = theHref <=< findLink <=< findClass "web"

parseAges :: Element -> Maybe String
parseAges = maybeStrContent . secondEl . findClasses "venue"
  where secondEl (_:x:xs) = Just x
        secondEl _ = Nothing
  
-- All SXSW 2011 events happen in 2011 in the CDT timezone. Local
-- times given after 11:59 p.m., but before, let's say, 6 a.m.,
-- technically occur on the next day; e.g., if the SXSW schedule says
-- "March 16 1:00AM," it means "March 17 1:00AM CDT."
parseStart :: Element -> Maybe UTCTime
parseStart xml = do
  cdtTime <- timeStr xml
  cdtDate <- dateStr xml
  cdtTimeOfDay <- toTimeOfDay cdtTime
  utct <- fmap (addUTCTime $ offset cdtTimeOfDay) $ toUTCTime $ cdtDate ++ " 2011 " ++ cdtTime ++ " CDT"
  return utct
  where
    offset tod
      | tod >= midnight && tod < morning = 60 * 60 * 24
      | otherwise                        = 0
    morning = TimeOfDay 6 0 0
    toUTCTime = parseTime defaultTimeLocale "%A %B %d %Y %l:%M %p %Z" :: String -> Maybe UTCTime
    toTimeOfDay = parseTime defaultTimeLocale "%l:%M %p" :: String -> Maybe TimeOfDay
