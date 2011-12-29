import Network.Curl
import Text.XML.Light
import Data.Maybe
import System.IO
import Data.Monoid
import Control.Monad

-- namespace used by SXSW schedule documents
ns = Just "http://www.w3.org/1999/xhtml"

eventURLsForDay day = do
  html <- unsafeCurlGetString $ scheduleForDay day
  let eventEls = onlyEvents . fromJust $ parseXMLDoc html
  return $ map (makeAbsoluteURLFrom . theHref) eventEls

scheduleForDay day = "http://schedule.sxsw.com/?conference=music&day=" ++ (show day) ++ "&category=Showcase#"

makeAbsoluteURLFrom u = "http://schedule.sxsw.com" ++ u

theHref = fromJust . findAttr hrefAttr
  where hrefAttr = QName "href" Nothing Nothing
        
findLinks = findElements linkEl
  where linkEl = QName "a" ns Nothing

onlyEvents = filterElements byEventEl
  where byEventEl x = (findAttr classAttr x) == Just "link_itemMusic"
        classAttr = QName "class" Nothing Nothing

unsafeCurlGetString url = curlGetString url [] >>= return . snd

main = do
  eventURLs <- mapM eventURLsForDay [15, 16, 17, 18, 19, 20]
  mapM_ putStrLn $ mconcat eventURLs
