import EventDetails
import Network.Curl
import Text.XML.Light
import qualified Data.Map as Map
import Data.Maybe

-- Parse event XML documents
getEventDoc url = fmap parseXMLDoc $ readFile "casiokids.html"

main = do
  xml <- getEventDoc "http://schedule.sxsw.com/events/event_MS14879"
  putStr $ Map.showTree $ eventDetails $ fromJust xml

unsafeCurlGetString url = curlGetString url [] >>= return . snd
