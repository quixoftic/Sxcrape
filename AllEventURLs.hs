import EventURLs
import System.IO
import Control.Monad
import Data.Monoid

main = do
  eventURLs <- mapM eventURLsForDay [15, 16, 17, 18, 19, 20]
  mapM_ putStrLn $ mconcat eventURLs
