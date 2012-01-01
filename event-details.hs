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

imgURL = theSrc <=< findImg <=< findClass "video_embed"
description = maybeStrContent . findClass "main_content_desc"
date = maybeStrContent . findClass "date"
time = maybeStrContent . findClass "time"

utcTime = parseTime defaultTimeLocale "%A %B %d %Y %l:%m %p %Z" :: String -> Maybe UTCTime

-- Note: all SXSW 2011 events happen in 2011 in the CDT timezone.
start xml = do
  t <- time xml
  d <- date xml
  utct <- utcTime $ d ++ " 2011 " ++ t ++ " CDT"
  return $ show utct

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
