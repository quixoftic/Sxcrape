{-# LANGUAGE DeriveDataTypeable, ViewPatterns, OverloadedStrings #-}
--
-- Module      : EventURLs
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Extract SXSW music event URLs from the daily schedules.
--

module EventURLs ( eventURLs
                 , eventURLsForDay
                 , eventFromURL
--                 , scheduleURLForDay
                 , Day(..)
                 ) where

import Network.HTTP.Conduit
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Encoding.Error as EncodingError
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
baseURL = "http://schedule.sxsw.com"

eventURLs :: IO [T.Text]
eventURLs = liftM mconcat $ mapM eventURLsForDay [Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

eventURLsForDay :: Day -> IO [T.Text]
eventURLsForDay day = do
  html <- simpleHttp (scheduleURLForDay day)
  return $ map makeAbsoluteURLFrom $ map (fromAttrib "href") $ filter (~== eventPattern) $ parseTags $ E.decodeUtf8With EncodingError.lenientDecode html

scheduleURLForDay :: Day -> String
scheduleURLForDay day = baseURL ++ "/?conference=music&day=" ++ (show (dayToDate day)) ++ "&event_type=showcase"

makeAbsoluteURLFrom :: T.Text -> T.Text
makeAbsoluteURLFrom u = (T.pack baseURL) `T.append` u

eventFromURL :: T.Text -> Maybe T.Text
eventFromURL (T.stripPrefix $ (T.pack baseURL) `T.append` "/2013/events/" -> Just event) = Just event
eventFromURL _ = Nothing

-- Map day-of-week names to SXSW 2013 March day-of-month numbers,
-- which are used by the SXSW schedule web service.
dayToDate :: Day -> Int
dayToDate day = 12 + fromEnum day -- Tuesday corresponds to Mar 12

eventPattern :: String
eventPattern = "<a class=\"more_details\">"
