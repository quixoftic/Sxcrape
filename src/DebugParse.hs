{-# LANGUAGE OverloadedStrings #-}

module DebugParse ( debugParse ) where

import Text.HTML.TagSoup
import qualified Data.Text.Lazy as T
import Data.ByteString.Lazy (ByteString)

type XMLTag = Tag T.Text
type XMLDoc = [XMLTag]

debugParse :: T.Text -> String
--debugParse xml = show (sections isTagText $ parseTags xml)
--debugParse xml = show (sections (~== TagText ("From"::String)) $ parseTags xml)
-- Get the From string.
--debugParse = T.unwords . T.words . fromTagText . (!! 2) . head . sections (~== (TagText ("From"::String))) . filter isTagText . parseTags
-- Get the description.
--debugParse = show . fromTagText . head . head . sections (~== ("<div class=\"block\">"::String)) . sections (~== ("<div class=\"data clearfix\">"::String)) . parseTags
--debugParse = show . innerText . takeWhile (~/= ("</div>"::String)) . dropWhile (~/= ("<div class=\"block\">"::String)) . dropWhile (~/= ("<div class=\"data clearfix\">"::String)) . parseTags
-- Get the artist URL
-- debugParse = show . fromAttrib "href" . head . dropWhile (~/= ("<a>"::String)) . head . sections (~== (TagText ("Online"::String))) . parseTags
-- Get the date string
debugParse = show . head . filter isTagText . head . sections (~== ("<h3 id=\"detail_time\">"::String)) . parseTags