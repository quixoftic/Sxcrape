import EventURLs
import System.IO
import Control.Monad
import Data.Monoid

main :: IO ()
main = eventURLs >>= mapM_ putStrLn
