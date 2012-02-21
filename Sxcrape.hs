{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import EventURLs
import Event
import System.Console.CmdArgs
import Data.Monoid
import Network.Curl
import Text.XML.Light
import Data.Maybe
import Data.Aeson.Generic (encode)
import Data.ByteString.Lazy as B hiding (map)

type URL = String

data Sxcrape = Events { day :: [Day] }
             | Details { urls :: FilePath
                       , write_to_file :: Bool
                       , events :: [URL]
                       }
             deriving (Typeable, Data, Eq, Show)

events_ = Events { day = def &= help "Get a specific day (default is all days)"
                 } &= help "Get music event URLs"
         
details_ = Details { urls = def &= typFile &= help "read event URLs from a file"
                   , write_to_file = def &= help "write JSON for each event to a separate file (default is to write them all to stdout)"
                   , events = def &= args &= typ "URL ..."
                   } &= help "Get music event details as JSON"
         
mode = cmdArgsMode $ modes [events_, details_] &= help "Scrape the SXSW music schedule" &= program "sxcrape" &= summary "sxcrape 0.1"

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  Events {} -> runEvents x
  Details {} -> runDetails x

runEvents :: Sxcrape -> IO ()
runEvents opts@(Events {}) = if (day opts) == []
                             then eventURLs >>= mapM_ Prelude.putStrLn
                             else mapM eventURLsForDay (day opts) >>= mapM_ Prelude.putStrLn . mconcat

runDetails :: Sxcrape -> IO ()
runDetails opts@Details {..}
  | events == [] = Prelude.putStrLn "details mode needs at least one event URL"
  | otherwise   = do
    jsonResults <- mapM eventDetailsAsJson events
    mapM_ B.putStrLn jsonResults

eventDetailsAsJson :: URL -> IO ByteString
eventDetailsAsJson url = do
  Just xml <- getEventDoc url
  return $ encode $ fromJust $ parseEvent xml

getEventDoc :: String -> IO (Maybe Element)
getEventDoc = fmap parseXMLDoc . unsafeCurlGetString

unsafeCurlGetString :: String -> IO String
unsafeCurlGetString url = curlGetString url [] >>= return . snd
