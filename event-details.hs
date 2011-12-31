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

theArtist = maybeStrContent . findClass "event_name"
theGenre =  maybeStrContent . findClass "event_sub_category"
theOrigin = maybeStrContent . findClass "event_citystate"
theArtistImg = theSrc <=< findImg <=< findClass "video_embed"
theDescription = maybeStrContent . findClass "main_content_desc"
theDate = maybeStrContent . findClass "date"
theTime = maybeStrContent . findClass "time"
theVenue = maybeStrContent . findLink <=< listToMaybe . findClasses "venue"
theAddress = maybeStrContent . findClass "address"  
theArtistHomepage = theHref <=< findLink <=< findClass "web"

theAges = maybeStrContent . secondEl . findClasses "venue"
  where secondEl (_:x:xs) = Just x
        secondEl _ = Nothing
  
eventDetails xml =
  let artist = theArtist xml
      genre = theGenre xml
      origin = theOrigin xml
      img = theArtistImg xml
      description = theDescription xml
      date = theDate xml
      time = theTime xml
      venue = theVenue xml
      ages = theAges xml
      addr = theAddress xml
      homepage = theArtistHomepage xml
  in Map.fromList [
    ("artist", artist),
    ("genre", genre),
    ("origin", origin),
    ("img", img),
    ("date", date),
    ("time", time),
    ("venue", venue),
    ("ages", ages),
    ("addr", addr),
    ("description", description),
    ("homepage", homepage)
    ]
  
main = do
  xml <- getEventDoc "http://schedule.sxsw.com/events/event_MS14879"
  putStr $ Map.showTree $ eventDetails $ fromJust xml

unsafeCurlGetString url = curlGetString url [] >>= return . snd
