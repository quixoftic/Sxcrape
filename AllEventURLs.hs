import EventURLs
import System.IO
import Control.Monad
import Data.Monoid

main :: IO ()
main = do
  eventURLs <- mapM eventURLsForDay [Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
  mapM_ putStrLn $ mconcat eventURLs
