{-# LANGUAGE OverloadedStrings #-}
--
-- Module      : Utility
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Utilities for the Sxred/Sxcrape tools.
--

module Utility ( readMultipleArgs
               , quietPrint
               , getContents'
               , download
               ) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Network.HTTP.Conduit hiding (def)
import qualified Data.Text.Lazy.Encoding as E

-- Read [String] from command line or from stdin, convert to [T.Text]
readMultipleArgs :: [String] -> IO [T.Text]
readMultipleArgs ("-":_) = T.getContents >>= return . T.lines
readMultipleArgs [] = T.getContents >>= return . T.lines
readMultipleArgs args = return $ map T.pack args

quietPrint :: Bool -> T.Text -> IO ()
quietPrint True s = T.putStrLn s
quietPrint False _ = return ()

getContents' :: T.Text -> IO T.Text
getContents' url
  | T.isPrefixOf "http://" url = download url
  | otherwise                  = T.readFile (T.unpack url)

download :: T.Text -> IO T.Text
download url = simpleHttp (T.unpack url) >>= return . E.decodeUtf8
