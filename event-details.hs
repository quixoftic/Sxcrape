import Network.Curl
import Text.XML.Light
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

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
maybeStrContent = fmap strContent

artist = maybeStrContent . findClass "event_name"
genre =  maybeStrContent . findClass "event_sub_category"
origin = maybeStrContent . findClass "event_citystate"
artistImg = theSrc <=< findImg <=< findClass "video_embed"
description = maybeStrContent . findClass "main_content_desc"
date = maybeStrContent . findClass "date"
time = maybeStrContent . findClass "time"
venue = maybeStrContent . findLink <=< listToMaybe . findClasses "venue"
address = maybeStrContent . findClass "address"
homepage = theHref <=< findLink <=< findClass "web"

ages = maybeStrContent . secondEl . findClasses "venue"
  where secondEl (_:x:xs) = Just x
        secondEl _ = Nothing
  
eventDetails xml = Map.fromList $ map (\(name,f) -> (name, f xml)) [("artist", artist),
                                                                    ("genre", genre),
                                                                    ("origin", origin),
                                                                    ("artistImg", artistImg),
                                                                    ("date", date),
                                                                    ("time", time),
                                                                    ("venue", venue),
                                                                    ("ages", ages),
                                                                    ("address", address),
                                                                    ("description", description),
                                                                    ("homepage", homepage)]

main = do
  xml <- getEventDoc "http://schedule.sxsw.com/events/event_MS14879"
  putStr $ Map.showTree $ eventDetails $ fromJust xml

unsafeCurlGetString url = curlGetString url [] >>= return . snd
