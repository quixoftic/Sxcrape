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
                   , start :: Maybe UTCTime
                   , end :: Maybe UTCTime
                   , ages :: T.Text
                   , genre :: T.Text
                   , description :: [T.Text]
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
        , ages = fromMaybe "Unknown" $ parseAges doc
        , genre = parseGenre doc
        , description = fromMaybe [] $ parseDescription doc
        , artistURL = fromMaybe "" $ parseArtistURL doc
        , origin = parseOrigin doc
        , imgURL = parseImgURL doc
        }

-- Parsing for each field.
--

-- A little trick to avoid all the ::String type signatures that are
-- needed with TagSoup and OverloadedString. Thanks to sclv at Stack
-- Overflow for the tip.
s :: String -> String
s = id

-- Origin often has weird formatting, so we scrub all the extraneous
-- formatting characters.
-- TODO: parse to "City, State" using "\n" as delimiter.
parseOrigin :: XMLDoc -> T.Text
parseOrigin = T.intercalate ", " . cleanLines . fromTagText . (!! 2) . head . sections (~== (TagText (s "From"))) . filter isTagText

-- Strip formatting characters (e.g., '\t') and blank lines, but
-- preserve overall paragraph structure.
parseDescription :: XMLDoc -> Maybe [T.Text]
parseDescription = liftM (filter (/= "") . map (T.unwords . T.words) . T.lines . innerText . takeWhile (~/= s "</div>")) . listToMaybe . sections (~== s "<div class=\"block\">") . takeWhile (~/= s "<!-- eo data -->") . dropWhile (~/= s "<div class=\"data clearfix\">")

parseVenue :: XMLDoc -> T.Text
parseVenue = scrubTagText . (!! 1) . dropWhile (~/= s "<h2 class=detail_venue>")

parseAges :: XMLDoc -> Maybe T.Text
parseAges = liftM (T.strip . fromJust . (T.stripPrefix "Age Policy:")) . listToMaybe . filter (T.isPrefixOf "Age Policy:") . map fromTagText . filter isTagText

parseImgURL :: XMLDoc -> T.Text
parseImgURL = fromAttrib "src" . (!! 0) . dropWhile (~/= s "<img>") . dropWhile (~/= s "<div class=video_embed>")

parseArtistURL :: XMLDoc -> Maybe T.Text
parseArtistURL = liftM (fromAttrib "href" . head . dropWhile (~/= s "<a>")) . listToMaybe . sections (~== (TagText (s "Online")))

parseGenre :: XMLDoc -> T.Text
parseGenre = scrubTagText . (!! 1) . dropWhile (~/= s "<a>") . head . sections (~== (TagText (s "Genre")))

parseArtist :: XMLDoc -> T.Text
parseArtist = scrubTagText . (!! 1) . dropWhile (~/= s "<title>")

parseAddress :: XMLDoc -> T.Text
parseAddress = scrubTagText . (!! 1) . dropWhile (~/= s "<h2 class=address>")

-- Parsing the start and end time is one big mess. Sorry.
--
parseStartTime :: XMLDoc -> Maybe UTCTime
parseStartTime xml = let cdtTime = parseStartTimeStr xml
                         cdtDate = parseDateStr xml in
                     fixUpDateAndTime cdtDate cdtTime

parseEndTime :: XMLDoc -> Maybe UTCTime
parseEndTime xml = let cdtTime = parseEndTimeStr xml
                       cdtDate = parseDateStr xml in
                   fixUpDateAndTime cdtDate cdtTime

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
dateAndTimeText = (\(date:time:_) -> (date, time)) . map scrubTagText . take 2 . filter isTagText . head . sections (~== s "<h3 id=\"detail_time\">")

parseDateStr :: XMLDoc -> T.Text
parseDateStr = fst . dateAndTimeText

-- The start and end time formatting is all over the place. Resort to
-- regexps.
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
scrubTagText :: Tag T.Text -> T.Text
scrubTagText = T.unwords . T.words . fromTagText

cleanLines :: T.Text -> [T.Text]
cleanLines = filter (/= "") . map (T.unwords . T.words) . T.lines