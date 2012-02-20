{-# LANGUAGE DeriveDataTypeable #-}

module EventURLs ( eventURLsForDay
                 , Day(..)
                 ) where

import Network.Curl
import Text.XML.Light
import Data.Maybe
import Control.Monad
import ParserUtils
import Data.Data

-- Note: only the music festival days!
data Day = Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
         deriving (Eq, Ord, Show, Read, Bounded, Enum, Data, Typeable)

eventURLsForDay :: Day -> IO [String]
eventURLsForDay day = do
  (_, html) <- curlGetString (scheduleURLForDay day) []
  let eventEls = findClasses "link_itemMusic" . fromMaybe blank_element $ parseXMLDoc html
  return $ map (makeAbsoluteURLFrom . fromJust . theHref) eventEls

scheduleURLForDay :: Day -> String
scheduleURLForDay day = "http://schedule.sxsw.com/2011/?conference=music&day=" 
                        ++ (show (dayToDate day)) 
                        ++ "&category=Showcase#"

makeAbsoluteURLFrom :: String -> String
makeAbsoluteURLFrom u = "http://schedule.sxsw.com/2011" ++ u

dayToDate :: Day -> Int
dayToDate Tuesday = 15
dayToDate Wednesday = 16
dayToDate Thursday = 17
dayToDate Friday = 18
dayToDate Saturday = 19
dayToDate Sunday = 20
