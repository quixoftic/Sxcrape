{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Event ( Event
             , parseEvent
             ) where

import Data.Maybe
import Control.Monad
import Data.Time as Time
import System.Locale
import Data.Data (Data, Typeable)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.UTF8
import Text.HTML.TagSoup
import qualified Data.Text.Lazy as T

type XMLTag = Tag T.Text
type XMLDoc = [XMLTag]

-- It's entirely possible that some events will be missing one or more
-- of these details. Those that are non-essential (e.g., artistURL)
-- might need to be changed to type Maybe T.Text. Let's see how it
-- goes.

data Event = Event { artist :: T.Text
                   , venue :: T.Text
                   , address :: T.Text
                   , start :: UTCTime
                   , end :: UTCTime
                   , ages :: T.Text
                   , genre :: T.Text
                   , description :: T.Text
                   , artistURL :: T.Text
                   , origin :: T.Text
                   , imgURL :: T.Text
                   } deriving (Show, Data, Typeable)

parseEvent :: T.Text -> Event
parseEvent xml = let doc = parseTags xml in
  Event { artist = parseArtist doc
        , venue = parseVenue doc
        , address = parseAddress doc
        , start = parseStartTime doc
        , end = parseEndTime doc
        , ages = parseAges doc
        , genre = parseGenre doc
        , description = parseDescription doc
        , artistURL = parseArtistURL doc
        , origin = parseOrigin doc
        , imgURL = parseImgURL doc
        }

scrubTagText :: Tag T.Text -> T.Text
scrubTagText = T.unwords . T.words . fromTagText

-- Origin often has weird formatting, so we scrub all the extraneous
-- formatting characters.
-- TODO: parse to "City, State" using "\n" as delimiter.
parseOrigin :: XMLDoc -> T.Text
parseOrigin = T.unwords . T.words . fromTagText . (!! 2) . head . sections (~== originPattern) . filter isTagText

-- Strip out the description line formatting.
-- TODO: preserve the <br/> tags for paragraph formatting.
parseDescription :: XMLDoc -> T.Text
parseDescription = T.intercalate " " . T.words . innerText . takeWhile (~/= ("</div>"::String)) . dropWhile (~/= ("<div class=\"block\">"::String)) . dropWhile (~/= ("<div class=\"data clearfix\">"::String))

parseVenue :: XMLDoc -> T.Text
parseVenue = textOf . (!! 1) . findFirst venuePattern

parseAges :: XMLDoc -> T.Text
parseAges = T.strip . fromJust . (T.stripPrefix "Age Policy:") . head . filter (T.isPrefixOf "Age Policy:") . map fromTagText . filter isTagText

parseImgURL :: XMLDoc -> T.Text
parseImgURL = fromAttrib "src" . (!! 0) . findFirst imgPattern . findFirst imgURLPattern

parseArtistURL :: XMLDoc -> T.Text
parseArtistURL = fromAttrib "href" . head . dropWhile (~/= ("<a>"::String)) . head . sections (~== (TagText ("Online"::String)))

parseGenre :: XMLDoc -> T.Text
parseGenre = textOf . (!! 1) . dropWhile (~/= ("<a>"::String)) . head . sections (~== (TagText ("Genre"::String)))

parseArtist :: XMLDoc -> T.Text
parseArtist = textOfFirst artistPattern

parseAddress :: XMLDoc -> T.Text
parseAddress = textOfFirst addressPattern

-- Parsing the start and end time is one big mess. Sorry.
--
parseStartTime :: XMLDoc -> UTCTime
parseStartTime xml = let cdtTime = parseStartTimeStr xml
                         cdtDate = parseDateStr xml in
                     fromJust $ fixUpDateAndTime cdtDate cdtTime

parseEndTime :: XMLDoc -> UTCTime
parseEndTime xml = let cdtTime = parseEndTimeStr xml
                       cdtDate = parseDateStr xml in
                   fromJust $ fixUpDateAndTime cdtDate cdtTime

-- All SXSW 2012 events happen in 2012 in the CDT timezone. Local
-- times given after 11:59 p.m., but before, let's say, 6 a.m.,
-- technically occur on the next day; e.g., if the SXSW schedule says
-- "March 16 1:00AM," it means "March 17 1:00AM CDT."
fixUpDateAndTime :: T.Text -> T.Text -> Maybe UTCTime
fixUpDateAndTime cdtDate cdtTime = do
  cdtTimeOfDay <- toTimeOfDay $ cdtTime
  utct <- fmap (addUTCTime $ offset cdtTimeOfDay) $ toUTCTime $ T.intercalate " " [cdtDate, "2012", cdtTime, "CDT"]
  return utct
  where
    offset tod
      | tod >= midnight && tod < morning = 60 * 60 * 24
      | otherwise                        = 0
    morning = TimeOfDay 6 0 0
    toUTCTime = parseTime defaultTimeLocale "%A, %B %d %Y %l:%M%p %Z" . T.unpack :: T.Text -> Maybe UTCTime
    toTimeOfDay = parseTime defaultTimeLocale "%l:%M%p" . T.unpack :: T.Text -> Maybe TimeOfDay

dateAndTimeText :: XMLDoc -> (T.Text, T.Text)
dateAndTimeText = (\(date:time:_) -> (date, time)) . map scrubTagText . take 2 . filter isTagText . head . sections (~== ("<h3 id=\"detail_time\">"::String))

parseDateStr :: XMLDoc -> T.Text
parseDateStr = fst . dateAndTimeText

startEndRegex :: String
startEndRegex = "[[:space:]]*-[[:space:]]*"

extractStartTimeStr :: T.Text -> T.Text
extractStartTimeStr str = (\(start, _, _) -> T.pack start) $ ((T.unpack str) =~ startEndRegex :: (String, String, String))

extractEndTimeStr :: T.Text -> T.Text
extractEndTimeStr str = (\(_, _, end) -> T.pack end) $ ((T.unpack str) =~ startEndRegex :: (String, String, String))

parseStartTimeStr :: XMLDoc -> T.Text
parseStartTimeStr = extractStartTimeStr . snd . dateAndTimeText

parseEndTimeStr :: XMLDoc -> T.Text
parseEndTimeStr = extractEndTimeStr . snd . dateAndTimeText

-- Parser helpers
--
textOf :: XMLTag -> T.Text
textOf = T.strip . fromTagText

findFirst :: String -> XMLDoc -> XMLDoc
findFirst pattern = dropWhile (~/= pattern)

textOfFirst :: String -> XMLDoc -> T.Text
textOfFirst pattern = textOf . (!! 1) . findFirst pattern

-- Patterns used in TagSoup interface to identify items of interest.
--
imgPattern :: String
imgPattern = "<img>"

linkPattern :: String
linkPattern = "<a>"

eventPattern :: String
eventPattern = "<a class=\"link_itemMusic\">"

originPattern :: Tag T.Text
originPattern = TagText "From"

descriptionPattern :: String
descriptionPattern = "<div class=\"data clearfix\">"

artistPattern :: String
artistPattern = "<title>"

artistURLPattern :: Tag T.Text
artistURLPattern = TagText "Online"

genrePattern :: String
genrePattern = "<h3 class=event_sub_category>"

imgURLPattern :: String
imgURLPattern = "<div class=video_embed>"

venuePattern :: String
venuePattern = "<h2 class=detail_venue>"

agesPattern :: String
agesPattern = "<h2 class=venue>"

addressPattern :: String
addressPattern = "<h2 class=address>"
