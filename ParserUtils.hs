module ParserUtils 
       ( findClass
       , findClasses
       , findImg
       , findLink
       , theSrc
       , theHref
       , maybeStrContent
       )  where

import Text.XML.Light
import Data.String.Utils as String

-- namespace used by SXSW schedule documents
ns = Just "http://www.w3.org/1999/xhtml"

byClass :: String -> Element -> Bool
byClass className el = (findAttr classAttr el) == Just className
  where classAttr = unqual "class"

findClass :: String -> Element -> Maybe Element
findClass className = filterElement (byClass className)

findClasses :: String -> Element -> [Element]
findClasses className = filterElements (byClass className)

findImg :: Element -> Maybe Element
findImg = findElement imgEl
  where imgEl = QName "img" ns Nothing

findLink :: Element -> Maybe Element
findLink = findElement linkEl
  where linkEl = QName "a" ns Nothing

theSrc :: Element -> Maybe String
theSrc = findAttr srcAttr
  where srcAttr = unqual "src"

theHref :: Element -> Maybe String
theHref = findAttr hrefAttr
  where hrefAttr = unqual "href"

maybeStrContent :: Maybe Element -> Maybe String
maybeStrContent = fmap strip . fmap strContent
