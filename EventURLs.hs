{-# LANGUAGE DeriveDataTypeable, ViewPatterns, OverloadedStrings #-}

module EventURLs ( eventURLs
                 , eventURLsForDay
                 , eventFromURL
                 , scheduleURLForDay
                 , Day(..)
                 ) where

import Network.Curl
import Data.Maybe
import Control.Monad
import Data.Monoid
import Data.Data
import Data.List
import Text.HTML.TagSoup

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
  return $ map makeAbsoluteURLFrom $ map (fromAttrib "href") $ filter (~== eventPattern) $ parseTags html

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

eventPattern :: String
eventPattern = "<a class=\"link_itemMusic\">"
