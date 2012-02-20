{-# LANGUAGE DeriveDataTypeable #-}
import EventURLs
import System.Console.CmdArgs
import Data.Monoid

data Sxcrape = Events { day :: [Day] }
             deriving (Typeable, Data, Eq, Show)

events = Events { day = def &= help "Get a specific day (default is all days)"
                } &= help "Get music event URLs"
         
mode = cmdArgsMode $ modes [events] &= help "Scrape the SXSW music schedule" &= program "sxcrape" &= summary "sxcrape 0.1"

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  Events {} -> runEvents x

runEvents :: Sxcrape -> IO ()
runEvents opts@(Events {}) = if (day opts) == []
                             then eventURLs >>= mapM_ putStrLn
                             else mapM eventURLsForDay (day opts) >>= mapM_ putStrLn . mconcat
