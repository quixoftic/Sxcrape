{-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

module EventURLs ( eventURLs
                 , eventURLsForDay
                 , eventFromURL
                 , Day(..)
                 ) where

import Network.Curl
import Text.XML.Light
import Data.Maybe
import Control.Monad
import Data.Monoid
import ParserUtils
import Data.Data
import Data.List

-- Note: only the music festival days!
data Day = Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
         deriving (Eq, Ord, Show, Read, Bounded, Enum, Data, Typeable)

-- Note: don't append the trailing slash!
baseURL :: String
baseURL = "http://schedule.sxsw.com/2011"

eventURLs :: IO [String]
eventURLs = liftM mconcat $ mapM eventURLsForDay [Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

eventURLsForDay :: Day -> IO [String]
eventURLsForDay day = do
  (_, html) <- curlGetString (scheduleURLForDay day) []
  let eventEls = findClasses "link_itemMusic" . fromMaybe blank_element $ parseXMLDoc html
  return $ map (makeAbsoluteURLFrom . fromJust . theHref) eventEls

scheduleURLForDay :: Day -> String
scheduleURLForDay day = baseURL ++ "/?conference=music&day=" ++ (show (dayToDate day)) ++ "&category=Showcase#"

makeAbsoluteURLFrom :: String -> String
makeAbsoluteURLFrom u = baseURL ++ u

eventFromURL :: String -> Maybe String
eventFromURL (stripPrefix $ baseURL ++ "/events/" -> Just event) = Just event
eventFromURL _ = Nothing

dayToDate :: Day -> Int
dayToDate Tuesday = 15
dayToDate Wednesday = 16
dayToDate Thursday = 17
dayToDate Friday = 18
dayToDate Saturday = 19
dayToDate Sunday = 20
