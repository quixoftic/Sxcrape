import EventURLs
import System.IO
import Control.Monad
import Data.Monoid
import Redis
import Database.Redis.Redis
import Data.Text.Lazy

main :: IO ()
main = do
  redis <- connect localhost defaultPort
  urls <- eventURLs
  eventIDs <- mapM (\id -> getOrSetEventID id redis) urls
  mapM_ (\id -> putStrLn $ show id) eventIDs
