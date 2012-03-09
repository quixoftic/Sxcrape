{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
import EventURLs
import Event
import System.Console.CmdArgs
import Data.Monoid
import Network.HTTP.Conduit hiding (def)
import Data.Maybe
import Data.Aeson.Generic (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8 (putStrLn)
import System.Directory
import Control.Monad
import Control.Exception (bracket)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Either

type URL = String

data Sxcrape = Events { day :: [Day] }
             | Dump { event :: URL }
             | MultiDump { output_dir :: Maybe FilePath,
                           urls :: FilePath }
             | Parse { events :: [URL] }
             deriving (Typeable, Data, Eq, Show)

events_ = record Events {} [ day := def += help "Get a specific day (default is all days)"
                           ] += help "Get music event URLs"

dump = record Dump {} [ event := def += argPos 0 += typ "URL"
                      ] += help "Download music event HTML"

multiDump = record MultiDump {} [ output_dir := def += typDir += help "output dump files in this directory"
                                , urls := def += argPos 0 += typFile
                                ] += help "Download multiple events from a file containing a list of URLs, and write the HTML of each to a separate file"

parse = record Parse {} [ events := def += args += typ "URL ..."
                        ] += help "Parse music event details into JSON"

mode = cmdArgsMode_ $ modes_ [events_, dump, multiDump, parse] += help "Scrape the SXSW music schedule" += program "sxcrape" += summary "sxcrape 0.1"

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  opts@Events {} -> runEvents (day opts)
  opts@Dump {} -> runDump $ T.pack $ event opts
  opts@MultiDump {} -> runMultiDump (urls opts) (output_dir opts)
  opts@Parse {} -> runParse $ map T.pack (events opts)

runEvents :: [Day] -> IO ()
runEvents [] = eventURLs >>= mapM_ T.putStrLn
runEvents days = mapM eventURLsForDay days >>= mapM_ T.putStrLn . mconcat

runDump :: T.Text -> IO ()
runDump event = download event >>= T.putStrLn

runMultiDump :: FilePath -> Maybe FilePath -> IO ()
runMultiDump urlsFile maybeDirName = do
  let outputDir = fromMaybe "." maybeDirName
  createDirectoryIfMissing True outputDir
  eventURLs <- fmap T.lines $ T.readFile urlsFile
  withCurrentDirectory outputDir $ do
    contents <- mapM download eventURLs
    mapM_ (\(url, contents) -> T.writeFile (urlToFilename url) contents) $ zip eventURLs contents

runParse :: [T.Text] -> IO ()
runParse [] = Prelude.putStrLn "Nothing to parse!"
runParse events = do
  jsonResults <- mapM eventDetailsAsJson events
  mapM_ C8.putStrLn jsonResults

eventDetailsAsJson :: T.Text -> IO ByteString
eventDetailsAsJson url = do
  xml <- download url
  return $ encode $ parseEvent xml

download :: T.Text -> IO T.Text
download url = do
  xml <- simpleHttp $ T.unpack url
  return $ E.decodeUtf8 xml

urlToFilename :: T.Text -> FilePath
urlToFilename = T.unpack . fromJust . eventFromURL

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dirName io =
  bracket
      (do
          cwd <- getCurrentDirectory
          when (dirName /= "") (setCurrentDirectory dirName)
          return cwd)
      (\orig -> setCurrentDirectory orig)
      (const io)
