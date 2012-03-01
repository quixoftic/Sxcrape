{-# LANGUAGE OverloadedStrings #-}

import Redis
import Database.Redis.Redis

main :: IO (Int)
main = connect localhost defaultPort >>= getOrSetEventID "MS1005"
