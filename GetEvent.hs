import Event
import Network.Curl
import Text.XML.Light
import Data.Maybe
import Data.Aeson.Generic (encode)
import Data.ByteString.Lazy as B
import Data.Data
import Data.Generics.Text

eventURL = "http://schedule.sxsw.com/2011/events/event_MS14879"

introspectData :: Data a => a -> [(String, String)]
introspectData a = Prelude.zip fields $ gmapQ gshow a
    where fields = constrFields $ toConstr a

main :: IO ()
main = do
  Just xml <- getEventDoc eventURL
  Prelude.putStrLn $ "Event details for " ++ eventURL ++ ":\n"
  B.putStrLn $ encode $ fromJust $ parseEvent xml
  Prelude.putStrLn $ show $ introspectData $ fromJust $ parseEvent xml

getEventDoc :: String -> IO (Maybe Element)
getEventDoc = fmap parseXMLDoc . unsafeCurlGetString

unsafeCurlGetString :: String -> IO String
unsafeCurlGetString url = curlGetString url [] >>= return . snd
