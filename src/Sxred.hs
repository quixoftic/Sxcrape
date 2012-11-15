{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
--
-- Module      : Main
-- Copyright   : Copyright © 2012, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Import scraped SXSW music event info into Redis.
--

import qualified Utility
import ParseEventDoc
import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Conduit hiding (def)
import Data.Monoid
import Redis
import Database.Redis
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as E
import System.Console.CmdArgs
import Paths_Sxcrape (version)
import Data.Version (showVersion)

type URL = String

data Sxred = BatchImport { urls :: [URL], 
                           quiet :: Bool }
           deriving (Typeable, Data, Eq, Show)

batchImport = record BatchImport { urls = def 
                                 , quiet = def } [ urls := def += args += typ "-|(URL|PATH ...)"
                                                 , quiet := def += help "don't echo URLs|PATHs on stdout"
                                                 ] += help "Parse events from one or more URLs given on the command line, or via stdin, and import into Redis."

mode = cmdArgsMode_ $ modes_ [batchImport] += help "Import the SXSW music schedule into Redis" += program "sxred" += summary ("sxred " ++ showVersion version)

main :: IO ()
main = cmdArgsRun mode >>= \x -> case x of
  opts@BatchImport {} -> do
    args <- Utility.readMultipleArgs (urls opts)
    runBatchImport args (quiet opts)

runBatchImport :: [T.Text] -> Bool -> IO ()
runBatchImport urls quiet = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    mapM_ (\url -> liftIO (Utility.quietPrint quiet url) >> liftIO (eventDetails url) >>= importEvent) urls

eventDetails :: T.Text -> IO Event
eventDetails url = Utility.getContents' url >>= return . parseEventDoc
