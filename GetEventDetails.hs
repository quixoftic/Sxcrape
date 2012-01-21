import Event
import Network.Curl
import Text.XML.Light
import qualified Data.Map as Map
import Data.Maybe

eventURL = "http://schedule.sxsw.com/2011/events/event_MS14879"

main :: IO ()
main = do
  Just xml <- getEventDoc eventURL
  putStrLn $ "Event details for " ++ eventURL ++ ":\n"
  mapM_ (\(k, v) -> putStrLn $ k ++ ": " ++ v) $ Map.toList $ eventDetails xml

getEventDoc :: String -> IO (Maybe Element)
getEventDoc = fmap parseXMLDoc . unsafeCurlGetString

unsafeCurlGetString :: String -> IO String
unsafeCurlGetString url = curlGetString url [] >>= return . snd
