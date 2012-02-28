{-# LANGUAGE DeriveDataTypeable #-}

module Event ( Event
             , parseEvent
             ) where

import Data.Maybe
import Control.Monad
import qualified Data.String.Utils as String (join, replace, strip)
import Data.Time as Time
import Locale as Locale
import Data.Data (Data, Typeable)
import Text.HTML.TagSoup

type XMLTag = Tag String
type XMLDoc = [XMLTag]

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

parseEvent :: String -> Event
parseEvent xml = let doc = parseTags xml in
  Event { artist = parseArtist doc
        , venue = parseVenue doc
        , address = parseAddress doc
        , start = fromJust $ parseStart doc
        , ages = parseAges doc
        , genre = parseGenre doc
        , description = parseDescription doc
        , artistURL = parseArtistURL doc
        , origin = parseOrigin doc
        , imgURL = parseImgURL doc
        }

-- Origin often has weird formatting.
parseOrigin :: XMLDoc -> String
parseOrigin = String.join ", " . words . textOfFirst originPattern

-- Strip out the description line formatting.
parseDescription :: XMLDoc -> String
parseDescription = String.replace "\n" "" . textOfFirst descriptionPattern

-- All SXSW 2011 events happen in 2011 in the CDT timezone. Local
-- times given after 11:59 p.m., but before, let's say, 6 a.m.,
-- technically occur on the next day; e.g., if the SXSW schedule says
-- "March 16 1:00AM," it means "March 17 1:00AM CDT."
parseStart :: XMLDoc -> Maybe UTCTime
parseStart xml = do
  let cdtTime = parseTimeStr xml
  let cdtDate = parseDateStr xml
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

-- The venue and ages fields are odd. For one thing, they both have
-- the same class ("venue").
parseVenue :: XMLDoc -> String
parseVenue = textOf . (!! 3) . findFirst venuePattern

parseAges :: XMLDoc -> String
parseAges = textOf . (!! 9) . findFirst agesPattern

-- The image and artist URLs also require some special-case code.
parseImgURL :: XMLDoc -> String
parseImgURL = fromAttrib "src" . (!! 0) . findFirst imgPattern . findFirst imgURLPattern

parseArtistURL :: XMLDoc -> String
parseArtistURL = fromAttrib "href" . (!! 0) . findFirst linkPattern . findFirst artistURLPattern

-- The remaining fields are all parsed the same way, save the pattern
-- used to find their elements.
parseArtist :: XMLDoc -> String
parseArtist = textOfFirst artistPattern

parseGenre :: XMLDoc -> String
parseGenre = textOfFirst genrePattern

parseDateStr :: XMLDoc -> String
parseDateStr = textOfFirst datePattern

parseTimeStr :: XMLDoc -> String
parseTimeStr = textOfFirst timePattern

parseAddress :: XMLDoc -> String
parseAddress = textOfFirst addressPattern


-- Parser helpers
--
textOf :: XMLTag -> String
textOf = String.strip . fromTagText

findFirst :: String -> XMLDoc -> XMLDoc
findFirst pattern = dropWhile (~/= pattern)

textOfFirst :: String -> XMLDoc -> String
textOfFirst pattern = textOf . (!! 1) . findFirst pattern

-- Patterns used in TagSoup interface to identify items of interest.
--
imgPattern :: String
imgPattern = "<img>"

linkPattern :: String
linkPattern = "<a>"

eventPattern :: String
eventPattern = "<a class=\"link_itemMusic\">"

originPattern :: String
originPattern = "<p class=event_citystate>"

descriptionPattern :: String
descriptionPattern = "<div class=main_content_desc>"

artistPattern :: String
artistPattern = "<h1 class=event_name>"

artistURLPattern :: String
artistURLPattern = "<h2 class=web>"

genrePattern :: String
genrePattern = "<h3 class=event_sub_category>"

datePattern :: String
datePattern = "<h2 class=date>"

imgURLPattern :: String
imgURLPattern = "<div class=video_embed>"

timePattern :: String
timePattern = "<h2 class=time>"

venuePattern :: String
venuePattern = "<h2 class=venue>"

agesPattern :: String
agesPattern = "<h2 class=venue>"

addressPattern :: String
addressPattern = "<h2 class=address>"
