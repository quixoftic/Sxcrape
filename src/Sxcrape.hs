{-# LANGUAGE DeriveDataTypeable, ViewPatterns, RecordWildCards, OverloadedStrings #-}
--
-- Module      : Main
-- Copyright   : Copyright © 2012, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Scrape the SXSW music event pages.
--

import qualified Utility
import EventURLs
import qualified ParseEventDoc
import System.Console.CmdArgs
import Data.Monoid
import Data.Maybe
import qualified Data.Aeson.Encode as Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8 (putStrLn, writeFile)
import System.Directory
import Control.Monad
import Control.Exception (bracket)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Either
import Paths_Sxcrape (version)
import Data.Version (showVersion)
import System.FilePath.Posix as Posix (takeBaseName, combine)
import Network.URI as URI (uriPath, parseURIReference)

type URL = String

data Sxcrape = Events { day :: [Day] }
             | Dump { event :: URL }
             | BatchDump { output_dir :: Maybe FilePath,
                           urls :: [URL], 
                           quiet :: Bool }
             | Parse { event :: URL }
             | BatchParse { output_dir :: Maybe FilePath,
                            urls :: [URL], 
                            quiet :: Bool }
             deriving (Typeable, Data, Eq, Show)

events_ = record Events { day = def } [ day := def += help "Get a specific day (default is all days)"
                                      ] += help "Get music event URLs"

dump = record Dump { event = def } [ event := def += argPos 0 += typ "URL"
                                   ] += help "Download music event HTML"

batchDump = record BatchDump { output_dir = def
                             , urls = def 
                             , quiet = def } [ output_dir := def += typDir += help "output dump files in this directory"
                                             , urls := def += args += typ "-|URL .."
                                             , quiet := def += help "don't echo URLs on stdout"
                                             ] += help "Download events from one or more URLs given on the command line, or via stdin; and write the HTML of each to a separate file"

parse = record Parse { event = def } [ event := def += argPos 0 += typ "URL|PATH"
                                     ] += help "Parse music event details into JSON, using a URL or the path to a file containing the event HTML"

batchParse = record BatchParse { output_dir = def
                               , urls = def 
                               , quiet = def } [ output_dir := def += typDir += help "output JSON files in this directory"
                                               , urls := def += args += typ "-|(URL|PATH ...)"
                                                 , quiet := def += help "don't echo URLs|PATHs on stdout"
                                                 ] += help "Parse events from one or more URLs given on the command line, or via stdin, into JSON, and store each in a separate file"

mode = cmdArgsMode_ $ modes_ [events_, dump, batchDump, parse, batchParse] += help "Scrape the SXSW music schedule" += program "sxcrape" += summary ("sxcrape " ++ showVersion version)

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  opts@Events {} -> runEvents (day opts)
  opts@Dump {} -> runDump $ T.pack $ event opts
  opts@BatchDump {} -> do
    args <- Utility.readMultipleArgs (urls opts)
    runBatchDump args (output_dir opts) (quiet opts)
  opts@Parse {} -> runParse $ T.pack (event opts)
  opts@BatchParse {} -> do
    args <- Utility.readMultipleArgs (urls opts)
    runBatchParse args (output_dir opts) (quiet opts)

runEvents :: [Day] -> IO ()
runEvents [] = eventURLs >>= mapM_ T.putStrLn
runEvents days = mapM eventURLsForDay days >>= mapM_ T.putStrLn . mconcat

runDump :: T.Text -> IO ()
runDump event = Utility.download event >>= T.putStrLn

runBatchDump :: [T.Text] -> Maybe FilePath -> Bool -> IO ()
runBatchDump urls maybeDirName quiet = do
  let outputDir = fromMaybe "." maybeDirName
  createDirectoryIfMissing True outputDir
  withCurrentDirectory outputDir $ do
    mapM_ (\url -> (Utility.quietPrint quiet url) >> Utility.download url >>= T.writeFile ((urlToFileName url) ++ ".html")) urls

runParse :: T.Text -> IO ()
runParse event = eventDetailsAsJson event >>= C8.putStrLn

runBatchParse :: [T.Text] -> Maybe FilePath -> Bool -> IO ()
runBatchParse urls maybeDirName quiet = do
  let outputDir = fromMaybe "." maybeDirName
  createDirectoryIfMissing True outputDir
  mapM_ (\url -> (Utility.quietPrint quiet url) >> eventDetailsAsJson url >>= C8.writeFile (combine outputDir $ (urlToFileName url) ++ ".json")) urls

eventDetailsAsJson :: T.Text -> IO ByteString
eventDetailsAsJson url = Utility.getContents' url >>= return . Aeson.encode . ParseEventDoc.parseEventDoc

urlToFileName :: T.Text -> FilePath
urlToFileName = takeBaseName . URI.uriPath . fromJust . URI.parseURIReference . T.unpack

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dirName io =
  bracket
      (do
          cwd <- getCurrentDirectory
          when (dirName /= "") (setCurrentDirectory dirName)
          return cwd)
      (\orig -> setCurrentDirectory orig)
      (const io)
