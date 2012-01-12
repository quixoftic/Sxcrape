module EventURLs (eventURLsForDay) where

import Network.Curl
import Text.XML.Light
import Data.Maybe
import Control.Monad
import ParserUtils

eventURLsForDay :: Num a => a -> IO [String]
eventURLsForDay day = do
  (_, html) <- curlGetString (scheduleForDay day) []
  let eventEls = findClasses "link_itemMusic" . fromMaybe blank_element $ parseXMLDoc html
  return $ map (makeAbsoluteURLFrom . fromJust . theHref) eventEls

scheduleForDay :: Num a => a -> String
scheduleForDay day = "http://schedule.sxsw.com/?conference=music&day=" ++ (show day) ++ "&category=Showcase#"

makeAbsoluteURLFrom :: String -> String
makeAbsoluteURLFrom u = "http://schedule.sxsw.com" ++ u
