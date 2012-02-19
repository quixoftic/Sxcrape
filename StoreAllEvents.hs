import EventURLs
import System.IO
import Control.Monad
import Data.Monoid
import Redis
import Database.Redis.Redis
import Data.Text (pack)

main :: IO ()
main = do
  redis <- connect localhost defaultPort
  eventURLs <- liftM mconcat $ mapM eventURLsForDay [15, 16, 17, 18, 19, 20]
  eventIDs <- mapM (\id -> getOrSetEventID (pack id) redis) eventURLs
  mapM_ (\id -> putStrLn $ show id) eventIDs