module Event (eventDetails) where

import Data.Maybe
import Control.Monad
import Data.String.Utils as String
import qualified Data.Map as Map
import Data.Time as Time
import Locale as Locale
import Text.XML.Light
import ParserUtils

eventDetails :: Element -> Map.Map String String
eventDetails xml = Map.mapMaybe id $
                   Map.fromList $ map (\(name,f) -> (name, f xml)) [("artist", artist),
                                                                    ("genre", genre),
                                                                    ("origin", origin),
                                                                    ("imgURL", imgURL),
                                                                    ("start", start),
                                                                    ("venue", venue),
                                                                    ("ages", ages),
                                                                    ("address", address),
                                                                    ("description", description),
                                                                    ("artistURL", artistURL)]

-- origin often has weird formatting.
origin :: Element -> Maybe String
origin = fmap (String.join ", " . words) . maybeStrContent . findClass "event_citystate"

-- Strip out the description line formatting.
description :: Element -> Maybe String
description = fmap (String.replace "\n" "") . maybeStrContent . findClass "main_content_desc"

artist :: Element -> Maybe String
artist = maybeStrContent . findClass "event_name"

genre :: Element -> Maybe String
genre =  maybeStrContent . findClass "event_sub_category"

imgURL :: Element -> Maybe String
imgURL = theSrc <=< findImg <=< findClass "video_embed"

dateStr :: Element -> Maybe String
dateStr = maybeStrContent . findClass "date"

timeStr :: Element -> Maybe String
timeStr = maybeStrContent . findClass "time"

venue :: Element -> Maybe String
venue = maybeStrContent . findLink <=< listToMaybe . findClasses "venue"

address :: Element -> Maybe String
address = maybeStrContent . findClass "address"

artistURL :: Element -> Maybe String
artistURL = theHref <=< findLink <=< findClass "web"

ages :: Element -> Maybe String
ages = maybeStrContent . secondEl . findClasses "venue"
  where secondEl (_:x:xs) = Just x
        secondEl _ = Nothing
  
-- All SXSW 2011 events happen in 2011 in the CDT timezone. Local
-- times given after 11:59 p.m., but before, let's say, 6 a.m.,
-- technically occur on the next day; e.g., if the SXSW schedule says
-- "March 16 1:00AM," it means "March 17 1:00AM CDT."
start :: Element -> Maybe String        
start xml = do
  cdtTime <- timeStr xml
  cdtDate <- dateStr xml
  cdtTimeOfDay <- toTimeOfDay cdtTime
  utct <- fmap (addUTCTime $ offset cdtTimeOfDay) $ toUTCTime $ cdtDate ++ " 2011 " ++ cdtTime ++ " CDT"
  return $ show utct
  where
    offset tod
      | tod >= midnight && tod < morning = 60 * 60 * 24
      | otherwise                        = 0
    morning = TimeOfDay 6 0 0
    toUTCTime = parseTime defaultTimeLocale "%A %B %d %Y %l:%M %p %Z" :: String -> Maybe UTCTime
    toTimeOfDay = parseTime defaultTimeLocale "%l:%M %p" :: String -> Maybe TimeOfDay
