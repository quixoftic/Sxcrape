{-# LANGUAGE DeriveDataTypeable, ViewPatterns, OverloadedStrings #-}

module EventURLs ( eventURLs
                 , eventURLsForDay
                 , eventFromURL
--                 , scheduleURLForDay
                 , Day(..)
                 ) where

import Network.Curl.Download.Lazy
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Maybe
import Control.Monad
import Data.Monoid
import Data.Data
import Data.List
import Text.HTML.TagSoup
import Text.StringLike

-- Note: only the music festival days!
data Day = Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
         deriving (Eq, Ord, Show, Read, Bounded, Enum, Data, Typeable)

-- Note: don't append the trailing slash!
baseURL :: String
baseURL = "http://schedule.sxsw.com/2011"

eventURLs :: IO [T.Text]
eventURLs = liftM mconcat $ mapM eventURLsForDay [Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

eventURLsForDay :: Day -> IO [T.Text]
eventURLsForDay day = do
  Right html <- openLazyURI (scheduleURLForDay day)
  return $ map makeAbsoluteURLFrom $ map (fromAttrib "href") $ filter (~== eventPattern) $ parseTags $ E.decodeUtf8 html

scheduleURLForDay :: Day -> String
scheduleURLForDay day = baseURL ++ "/?conference=music&day=" ++ (show (dayToDate day)) ++ "&category=Showcase#"

makeAbsoluteURLFrom :: T.Text -> T.Text
makeAbsoluteURLFrom u = (T.pack baseURL) `T.append` u

eventFromURL :: T.Text -> Maybe T.Text
eventFromURL (T.stripPrefix $ (T.pack baseURL) `T.append` "/events/" -> Just event) = Just event
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
