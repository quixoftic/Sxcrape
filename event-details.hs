import Network.Curl
import Text.XML.Light
import Data.Maybe
import Control.Monad
import Data.String.Utils as String
import qualified Data.Map as Map
import Data.Time as Time
import Locale as Locale

-- namespace used by SXSW schedule documents
ns = Just "http://www.w3.org/1999/xhtml"

-- Parse event XML documents
getEventDoc url = fmap parseXMLDoc $ readFile "casiokids.html"

-- Extract event details
byClass className el = (findAttr classAttr el) == Just className
  where classAttr = unqual "class"

findClass className = filterElement (byClass className)
findClasses className = filterElements (byClass className)

findImg = findElement imgEl
  where imgEl = QName "img" ns Nothing

findLink = findElement linkEl
  where linkEl = QName "a" ns Nothing

theSrc = findAttr srcAttr
  where srcAttr = unqual "src"

theHref = findAttr hrefAttr
  where hrefAttr = unqual "href"
    
maybeStrContent :: Maybe Element -> Maybe String
maybeStrContent = fmap strip . fmap strContent

artist = maybeStrContent . findClass "event_name"
genre =  maybeStrContent . findClass "event_sub_category"

-- origin often has weird formatting.
origin = fmap (String.join ", ") . fmap splitWs . maybeStrContent . findClass "event_citystate"

-- Strip out the description line formatting.
description = fmap (String.replace "\n" "") . maybeStrContent . findClass "main_content_desc"

imgURL = theSrc <=< findImg <=< findClass "video_embed"
dateStr = maybeStrContent . findClass "date"
timeStr = maybeStrContent . findClass "time"

-- All SXSW 2011 events happen in 2011 in the CDT timezone. Local
-- times given after 11:59 p.m., but before, let's say, 6 a.m.,
-- technically occur on the next day; e.g., if the SXSW schedule says
-- "March 16 1:00AM," it means "March 17 1:00AM CDT."
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

venue = maybeStrContent . findLink <=< listToMaybe . findClasses "venue"
address = maybeStrContent . findClass "address"
artistURL = theHref <=< findLink <=< findClass "web"

ages = maybeStrContent . secondEl . findClasses "venue"
  where secondEl (_:x:xs) = Just x
        secondEl _ = Nothing
  
eventDetails xml = Map.fromList $ map (\(name,f) -> (name, f xml)) [("artist", artist),
                                                                    ("genre", genre),
                                                                    ("origin", origin),
                                                                    ("imgURL", imgURL),
                                                                    ("start", start),
                                                                    ("venue", venue),
                                                                    ("ages", ages),
                                                                    ("address", address),
                                                                    ("description", description),
                                                                    ("artistURL", artistURL)]

main = do
  xml <- getEventDoc "http://schedule.sxsw.com/events/event_MS14879"
  putStr $ Map.showTree $ eventDetails $ fromJust xml

unsafeCurlGetString url = curlGetString url [] >>= return . snd
