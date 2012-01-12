module ParserUtils where

import Text.XML.Light
import Data.String.Utils as String

-- namespace used by SXSW schedule documents
ns = Just "http://www.w3.org/1999/xhtml"

byClass className el = (findAttr classAttr el) == Just className
  where classAttr = unqual "class"

findClass className = filterElement (byClass className)
findClasses className = filterElements (byClass className)

findImg = findElement imgEl
  where imgEl = QName "img" ns Nothing

findLink = findElement linkEl
  where linkEl = QName "a" ns Nothing

theSrc = findAttr srcAttr
  where srcAttr = unqual "src"

theHref = findAttr hrefAttr
  where hrefAttr = unqual "href"

maybeStrContent :: Maybe Element -> Maybe String
maybeStrContent = fmap strip . fmap strContent
