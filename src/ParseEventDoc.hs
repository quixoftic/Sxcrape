{-# LANGUAGE OverloadedStrings #-}
--
-- Module      : ParseEventDoc
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Parse SXSW music event pages.
--

module ParseEventDoc (parseEventDoc) where

import qualified Event as E
import qualified Artist as A
import qualified Venue as V
import Data.Maybe
import Control.Monad
import Data.Time as Time
import System.Locale
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.UTF8
import Text.HTML.TagSoup
import qualified Data.Text.Lazy as T
import Network.URI as URI (isRelativeReference)

type XMLTag = Tag T.Text
type XMLDoc = [XMLTag]

parseEventDoc :: T.Text -> (E.Event, A.Artist, V.Venue)
parseEventDoc xml = let doc = parseTags xml in
  ( E.Event { E.url = parseURL doc
            , E.artist = parseArtist doc
            , E.venue = parseVenue doc
            , E.day = parseDay doc
            , E.start = parseStartTime doc
            , E.end = parseEndTime doc
            , E.ages = parseAges doc
            , E.hashTags = fromMaybe [] $ parseHashTags doc
          }
   , A.Artist { A.name = parseArtist doc
              , A.url = parseArtistURL doc
              , A.genre = parseGenre doc
              , A.origin = parseOrigin doc
              , A.imgURL = parseImgURL doc
              , A.songURL = parseSongURL doc
              , A.videoURL = parseVideoURL doc
              }
     , V.Venue { V.name = parseVenue doc
               , V.address = parseAddress doc
               }
     )

-- Parsing for each field.
--

-- A little trick to avoid all the ::String type signatures that are
-- needed with TagSoup and OverloadedString. Thanks to sclv at Stack
-- Overflow for the tip.
s :: String -> String
s = id

-- Clients will want the original URL. We can't assume that the
-- original URL is available out-of-band, because the parser may be
-- operating on a locally cached resource, e.g., a file on the
-- filesystem. It happens to be contained in the event HTML, so for
-- consistency we grab it from there.
parseURL :: XMLDoc -> T.Text
parseURL = fromAttrib "data-url" . head . head . sections (~== (TagOpen (s "a") [("href", "http://twitter.com/share")]))

-- Origin often has weird formatting, so we scrub all the extraneous
-- formatting characters. Origin may also be the empty string, so wrap
-- it in a Maybe.
parseOrigin :: XMLDoc -> Maybe T.Text
parseOrigin = textToMaybe . T.intercalate ", " . cleanLines . fromTagText . (!! 2) . head . sections (~== (TagText (s "From"))) . filter isTagText

-- Strip formatting characters (e.g., '\t') and blank lines from
-- description, but preserve overall paragraph structure.
--
-- Note: not currently used, but left here for reference.
parseDescription :: XMLDoc -> Maybe [T.Text]
parseDescription = liftM (filter (/= "") . map (T.unwords . T.words) . T.lines . innerText . takeWhile (~/= s "</div>")) . listToMaybe . sections (~== s "<div class=\"block\">") . takeWhile (~/= s "<!-- eo data -->") . dropWhile (~/= s "<div class=\"data clearfix\">")

parseVenue :: XMLDoc -> T.Text
parseVenue = scrubTagText . (!! 1) . dropWhile (~/= s "<h4 class=detail_venue>")

parseAges :: XMLDoc -> Maybe T.Text
parseAges = liftM (T.strip . fromJust . (T.stripPrefix "Age Policy:")) . listToMaybe . filter (T.isPrefixOf "Age Policy:") . map fromTagText . filter isTagText

-- Some artist image URLs, specifically the placeholder "Showcasing
-- Artist" one, are relative to http://schedule.sxsw.com/. Fix those
-- up.
parseImgURL :: XMLDoc -> T.Text
parseImgURL = makeAbsolute . fromAttrib "src" . (!! 0) . dropWhile (~/= s "<img>") . dropWhile (~/= s "<div class=video_embed>")
  where
    makeAbsolute url
      | URI.isRelativeReference (T.unpack url) = T.concat ["http://schedule.sxsw.com", url]
      | otherwise                              = url

parseSongURL :: XMLDoc -> Maybe T.Text
parseSongURL = liftM (T.takeWhile (/= '&') . fromJust . T.stripPrefix "file=" . fromAttrib "value" . head) . listToMaybe . sections (~== s "<param name=\"flashvars\">")

parseVideoURL :: XMLDoc -> Maybe T.Text
parseVideoURL = liftM (fromAttrib "src" . head . dropWhile (~/= s "<iframe>")) . listToMaybe . sections (~== (TagText (s "Music Video")))
  
parseArtistURL :: XMLDoc -> Maybe T.Text
parseArtistURL = liftM (fromAttrib "href" . head . dropWhile (~/= s "<a>")) . listToMaybe . sections (~== (TagText (s "Online")))

parseHashTags :: XMLDoc -> Maybe [T.Text]
parseHashTags = liftM (T.splitOn " " . scrubTagText) . maybeHashTags . filter isTagText . takeWhile (~/= s "</div>") . dropWhile (~/= s "<div class=\"meta clearfix\">")
  where
    maybeHashTags :: [Tag T.Text] -> Maybe (Tag T.Text)
    maybeHashTags (_:x:_) = Just x
    maybeHashTags _       = Nothing

-- Genre may be the empty string, so wrap it in a Maybe.
parseGenre :: XMLDoc -> Maybe T.Text
parseGenre = textToMaybe . scrubTagText . head  . filter isTagText . dropWhile (~/= s "<a>") . head . sections (~== (TagText (s "Genre")))

parseArtist :: XMLDoc -> T.Text
parseArtist = scrubTagText . (!! 2) . dropWhile (~/= s "<div class=data>")

parseAddress :: XMLDoc -> T.Text
parseAddress = scrubTagText . (!! 1) . dropWhile (~/= s "<h4 class=address>")

-- Note: the day given in the event info is technically one day
-- earlier than the actual day, for events whose start time occurs
-- after midnight CDT.
parseDay :: XMLDoc -> T.Text
parseDay doc = T.pack $ showGregorian $ fromJust $ toDay $ T.intercalate " " [parseDateStr doc, "2013"]
  where
    toDay = parseTime defaultTimeLocale "%A, %B %d %Y" . T.unpack :: T.Text -> Maybe Day

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

-- All SXSW 2013 events happen in 2013 in the CDT timezone. Local
-- times given after 11:59 p.m., but before, let's say, 6 a.m.,
-- technically occur on the next day; e.g., if the SXSW schedule says
-- "March 16 1:00AM," it means "March 17 1:00AM CDT."
fixUpDateAndTime :: T.Text -> T.Text -> Maybe UTCTime
fixUpDateAndTime cdtDate cdtTime = do
  cdtTimeOfDay <- toTimeOfDay $ cdtTime
  utct <- fmap (addUTCTime $ offset cdtTimeOfDay) $ toUTCTime $ T.intercalate " " [cdtDate, "2013", cdtTime, "CDT"]
  return utct
  where
    offset tod
      | tod >= midnight && tod < morning = 60 * 60 * 24
      | otherwise                        = 0
    morning = TimeOfDay 6 0 0
    toUTCTime = parseTime defaultTimeLocale "%A, %B %d %Y %l:%M%p %Z" . T.unpack :: T.Text -> Maybe UTCTime
    toTimeOfDay = parseTime defaultTimeLocale "%l:%M%p" . T.unpack :: T.Text -> Maybe TimeOfDay

dateAndTimeText :: XMLDoc -> (T.Text, T.Text)
dateAndTimeText = (\(_:date:time:_) -> (date, time)) . map scrubTagText . take 3 . filter isTagText . head . sections (~== s "<h3 id=\"detail_time\">")

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
textToMaybe :: T.Text -> Maybe T.Text
textToMaybe "" = Nothing
textToMaybe text = Just text

scrubTagText :: Tag T.Text -> T.Text
scrubTagText = T.unwords . T.words . fromTagText

cleanLines :: T.Text -> [T.Text]
cleanLines = filter (/= "") . map (T.unwords . T.words) . T.lines