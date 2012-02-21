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
             | Dump { event :: URL }
             | Parse { events :: [URL] }
             deriving (Typeable, Data, Eq, Show)

events_ = Events { day = def &= help "Get a specific day (default is all days)"
                 } &= help "Get music event URLs"
         
dump = Dump { event = def &= argPos 0 &= typ "URL"
            } &= help "Download music event HTML"

parse = Parse { events = def &= args &= typ "URL ..."
              } &= help "Parse music event details into JSON"
         
mode = cmdArgsMode $ modes [events_, dump, parse] &= help "Scrape the SXSW music schedule" &= program "sxcrape" &= summary "sxcrape 0.1"

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  Events {} -> runEvents x
  Dump {} -> runDump x
  Parse {} -> runParse x

runEvents :: Sxcrape -> IO ()
runEvents opts@(Events {}) = if (day opts) == []
                             then eventURLs >>= mapM_ Prelude.putStrLn
                             else mapM eventURLsForDay (day opts) >>= mapM_ Prelude.putStrLn . mconcat

runDump :: Sxcrape -> IO ()
runDump opts@Dump {..} = unsafeCurlGetString event >>= Prelude.putStrLn

runParse :: Sxcrape -> IO ()
runParse opts@Parse {..}
  | events == [] = Prelude.putStrLn "Nothing to parse!"
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
