import Event
import Network.Curl
import Text.XML.Light
import Data.Maybe

eventURL = "http://schedule.sxsw.com/2011/events/event_MS14879"

main :: IO ()
main = do
  Just xml <- getEventDoc eventURL
  putStrLn $ "Event details for " ++ eventURL ++ ":\n"
  print $ parseEvent xml

getEventDoc :: String -> IO (Maybe Element)
getEventDoc = fmap parseXMLDoc . unsafeCurlGetString

unsafeCurlGetString :: String -> IO String
unsafeCurlGetString url = curlGetString url [] >>= return . snd
