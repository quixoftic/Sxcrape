module EventURLs (eventURLsForDay) where

import Network.Curl
import Text.XML.Light
import Data.Maybe

-- namespace used by SXSW schedule documents
ns = Just "http://www.w3.org/1999/xhtml"

eventURLsForDay day = do
  html <- unsafeCurlGetString $ scheduleForDay day
  let eventEls = onlyEvents . fromJust $ parseXMLDoc html
  return $ map (makeAbsoluteURLFrom . theHref) eventEls

scheduleForDay day = "http://schedule.sxsw.com/?conference=music&day=" ++ (show day) ++ "&category=Showcase#"

makeAbsoluteURLFrom u = "http://schedule.sxsw.com" ++ u

theHref = fromJust . findAttr hrefAttr
  where hrefAttr = unqual "href"
        
findLinks = findElements linkEl
  where linkEl = QName "a" ns Nothing

onlyEvents = filterElements byEventEl
  where byEventEl x = (findAttr classAttr x) == Just "link_itemMusic"
        classAttr = unqual "class"

unsafeCurlGetString url = curlGetString url [] >>= return . snd
