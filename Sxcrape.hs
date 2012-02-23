{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import EventURLs
import Event
import System.Console.CmdArgs
import Data.Monoid
import Network.Curl
import Text.HTML.TagSoup
import Data.Maybe
import Data.Aeson.Generic (encode)
import Data.ByteString.Lazy as B (putStrLn, ByteString)
import System.Directory
import Control.Monad
import Control.Exception (bracket)

type URL = String

data Sxcrape = Events { day :: [Day] }
             | Dump { event :: URL }
             | MultiDump { output_dir :: Maybe FilePath,
                           urls :: FilePath }
             | Parse { events :: [URL] }
             deriving (Typeable, Data, Eq, Show)

events_ = Events { day = def &= help "Get a specific day (default is all days)"
                 } &= help "Get music event URLs"

dump = Dump { event = def &= argPos 0 &= typ "URL"
            } &= help "Download music event HTML"

multiDump = MultiDump { output_dir = def &= typDir &= help "output dump files in this directory"
                      , urls = def &= argPos 0 &= typFile
                      } &= help "Download multiple events from a file containing a list of URLs, and write the HTML of each to a separate file"

parse = Parse { events = def &= args &= typ "URL ..."
              } &= help "Parse music event details into JSON"

mode = cmdArgsMode $ modes [events_, dump, multiDump, parse] &= help "Scrape the SXSW music schedule" &= program "sxcrape" &= summary "sxcrape 0.1"

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  Events {} -> runEvents x
  Dump {} -> runDump x
  MultiDump {} -> runMultiDump x
  Parse {} -> runParse x

runEvents :: Sxcrape -> IO ()
runEvents opts@(Events {}) = if (day opts) == []
                             then eventURLs >>= mapM_ Prelude.putStrLn
                             else mapM eventURLsForDay (day opts) >>= mapM_ Prelude.putStrLn . mconcat

runDump :: Sxcrape -> IO ()
runDump opts@Dump {..} = unsafeCurlGetString event >>= Prelude.putStrLn

runMultiDump :: Sxcrape -> IO ()
runMultiDump opts@MultiDump {..} = do
  let outputDir = case output_dir of
        Just dirName -> dirName
        Nothing -> "."
  createDirectoryIfMissing True outputDir
  eventURLs <- fmap lines $ readFile urls
  withCurrentDirectory outputDir $ do
    contents <- mapM unsafeCurlGetString eventURLs
    mapM_ (\(url, contents) -> writeFile (urlToFilename url) contents) $ zip eventURLs contents

runParse :: Sxcrape -> IO ()
runParse opts@Parse {..}
  | events == [] = Prelude.putStrLn "Nothing to parse!"
  | otherwise   = do
    jsonResults <- mapM eventDetailsAsJson events
    mapM_ B.putStrLn jsonResults

eventDetailsAsJson :: URL -> IO ByteString
eventDetailsAsJson url = do
  xml <- getEventDoc url
  return $ encode $ parseEvent xml

getEventDoc :: String -> IO [Tag String]
getEventDoc xml = unsafeCurlGetString xml >>= return . parseTags

unsafeCurlGetString :: String -> IO String
unsafeCurlGetString url = curlGetString url [] >>= return . snd

urlToFilename :: URL -> FilePath
urlToFilename = fromJust . eventFromURL

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dirName io =
  bracket
      (do
          cwd <- getCurrentDirectory
          when (dirName /= "") (setCurrentDirectory dirName)
          return cwd)
      (\orig -> setCurrentDirectory orig)
      (const io)
