import EventURLs
import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Redis
import Database.Redis
import Data.Text.Lazy

main :: IO ()
main = do
  urls <- eventURLs
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    eventIDs <- mapM (\id -> getOrSetEventID id) urls
    liftIO $ mapM_ (\id -> putStrLn $ show id) eventIDs
