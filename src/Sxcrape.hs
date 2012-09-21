{-# LANGUAGE DeriveDataTypeable, ViewPatterns, RecordWildCards, OverloadedStrings #-}
import EventURLs
import Event
import System.Console.CmdArgs
import Data.Monoid
import Network.HTTP.Conduit hiding (def)
import Data.Maybe
import qualified Data.Aeson.Generic as Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8 (putStrLn, writeFile)
import System.Directory
import Control.Monad
import Control.Exception (bracket)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Either
import Paths_Sxcrape (version)
import Data.Version (showVersion)
import System.FilePath.Posix as Posix (splitFileName, splitExtension)
import Network.URI as URI (uriPath, parseURIReference)

type URL = String

data Sxcrape = Events { day :: [Day] }
             | Dump { event :: URL }
             | BatchDump { output_dir :: Maybe FilePath,
                           urls :: [URL] }
             | Parse { event :: URL }
             | BatchParse { output_dir :: Maybe FilePath,
                            urls :: [URL] }
             deriving (Typeable, Data, Eq, Show)

events_ = record Events { day = def } [ day := def += help "Get a specific day (default is all days)"
                                      ] += help "Get music event URLs"

dump = record Dump { event = def } [ event := def += argPos 0 += typ "URL"
                                   ] += help "Download music event HTML"

batchDump = record BatchDump { output_dir = def
                             , urls = def } [ output_dir := def += typDir += help "output dump files in this directory"
                                            , urls := def += args += typ "-|URL .."
                                            ] += help "Download events from one or more URLs given on the command line, or via stdin; and write the HTML of each to a separate file"

parse = record Parse { event = def } [ event := def += argPos 0 += typ "URL|PATH"
                                     ] += help "Parse music event details into JSON, using a URL or the path to a file containing the event HTML"

batchParse = record BatchParse { output_dir = def
                               , urls = def } [ output_dir := def += typDir += help "output JSON files in this directory"
                                              , urls := def += args += typ "-|(URL|PATH ...)"
                                              ] += help "Parse events from one or more URLs given on the command line, or via stdin, into JSON, and store each in a separate file"

mode = cmdArgsMode_ $ modes_ [events_, dump, batchDump, parse, batchParse] += help "Scrape the SXSW music schedule" += program "sxcrape" += summary ("sxcrape " ++ showVersion version)

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  opts@Events {} -> runEvents (day opts)
  opts@Dump {} -> runDump $ T.pack $ event opts
  opts@BatchDump {} -> do
    args <- readMultipleArgs (urls opts)
    runBatchDump args (output_dir opts)
  opts@Parse {} -> runParse $ T.pack (event opts)
  opts@BatchParse {} -> do
    args <- readMultipleArgs (urls opts)
    runBatchParse args (output_dir opts)

-- Read [String] from command line or stdin, convert to [T.Text]
readMultipleArgs :: [String] -> IO [T.Text]
readMultipleArgs ("-":_) = T.getContents >>= return . T.lines
readMultipleArgs [] = T.getContents >>= return . T.lines
readMultipleArgs args = return $ map T.pack args

runEvents :: [Day] -> IO ()
runEvents [] = eventURLs >>= mapM_ T.putStrLn
runEvents days = mapM eventURLsForDay days >>= mapM_ T.putStrLn . mconcat

runDump :: T.Text -> IO ()
runDump event = download event >>= T.putStrLn

runBatchDump :: [T.Text] -> Maybe FilePath -> IO ()
runBatchDump urls maybeDirName = do
  let outputDir = fromMaybe "." maybeDirName
  createDirectoryIfMissing True outputDir
  withCurrentDirectory outputDir $ do
    contents <- mapM download urls
    mapM_ (\(url, contents) -> T.writeFile ((urlToFileName url) ++ ".html") contents) $ zip urls contents

runParse :: T.Text -> IO ()
runParse event = eventDetailsAsJson event >>= C8.putStrLn

runBatchParse :: [T.Text] -> Maybe FilePath -> IO ()
runBatchParse urls maybeDirName = do
  let outputDir = fromMaybe "." maybeDirName
  createDirectoryIfMissing True outputDir
  -- Some of the input URLs may be local files; read them relative to
  -- the program's working directory, not the output directory.
  contents <- mapM eventDetailsAsJson urls
  withCurrentDirectory outputDir $ do
    mapM_ (\(url, contents) -> C8.writeFile ((urlToFileName url) ++ ".json") contents) $ zip urls contents

eventDetailsAsJson :: T.Text -> IO ByteString
eventDetailsAsJson url = getContents url >>= return . Aeson.encode . parseEvent
  where
    getContents url
      | T.isPrefixOf "http://" url = download url
      | otherwise                  = T.readFile (T.unpack url)

download :: T.Text -> IO T.Text
download url = simpleHttp (T.unpack url) >>= return . E.decodeUtf8

urlToFileName :: T.Text -> FilePath
urlToFileName = fst . Posix.splitExtension . snd . Posix.splitFileName . URI.uriPath . fromJust . URI.parseURIReference . T.unpack

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dirName io =
  bracket
      (do
          cwd <- getCurrentDirectory
          when (dirName /= "") (setCurrentDirectory dirName)
          return cwd)
      (\orig -> setCurrentDirectory orig)
      (const io)
