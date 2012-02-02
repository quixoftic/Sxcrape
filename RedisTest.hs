{-# LANGUAGE OverloadedStrings #-}

import Database.Redis.Redis (Redis, Reply, connect, localhost, defaultPort, ping, incr)
import Database.Redis.ByteStringClass (BS, toBS, fromBS)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

version :: T.Text
version = "1"

instance BS T.Text where
  toBS = E.encodeUtf8
  fromBS = E.decodeUtf8

insertKey :: Redis -> IO (Reply Int)
insertKey r = incr r ("test:key" :: T.Text)

main :: IO (Reply Int)
main = connect localhost defaultPort >>= insertKey
