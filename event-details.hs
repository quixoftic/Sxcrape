import Network.Curl
import Text.XML.Light
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

-- namespace used by SXSW schedule documents
ns = Just "http://www.w3.org/1999/xhtml"

eventDetails url = do
  --xml <- liftM (fromJust . parseXMLDoc) $ unsafeCurlGetString url
  xml <- liftM (fromJust . parseXMLDoc) $ readFile "casiokids.html"
  let artist = strContent $ findClass "event_name" xml
      genre = strContent $ findClass "event_sub_category" xml
      origin = strContent $ findClass "event_citystate" xml
      img = theSrc $ findImg $ findClass "video_embed" xml
      description = strContent $ findClass "main_content_desc" xml
      date = strContent $ findClass "date" xml
      time = strContent $ findClass "time" xml
      venue =  strContent . findLink . head $ findClasses "venue" xml
      age = strContent $ findClasses "venue" xml !! 1
      addr = strContent $ findClass "address" xml
      homepage = theHref . findLink $ findClass "web" xml
  return $ Map.fromList [
    ("artist", artist),
    ("genre", genre),
    ("origin", origin),
    ("img", img),
    ("date", date),
    ("time", time),
    ("venue", venue),
    ("age", age),
    ("addr", addr),
    ("description", description),
    ("homepage", homepage)
    ]
  where findClass name doc = fromJust $ filterElement (byClass name) doc
        findClasses name doc = filterElements (byClass name) doc
        findImg = fromJust . findElement imgEl
        theSrc = fromJust . findAttr srcAttr
        byClass name el = (findAttr classAttr el) == Just name
        theHref = fromJust . findAttr hrefAttr
        hrefAttr = unqual "href"
        findLink = fromJust . findElement linkEl
        linkEl = QName "a" ns Nothing
        imgEl = QName "img" ns Nothing
        classAttr = unqual "class"
        srcAttr = unqual "src"
  
main = do
  mapRep <- liftM Map.showTree $ eventDetails "http://schedule.sxsw.com/events/event_MS14879"
  putStr mapRep

unsafeCurlGetString url = curlGetString url [] >>= return . snd
