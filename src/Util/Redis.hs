{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Util.Redis where

import Import
import Database.Redis

notFetchedRecently :: Connection -> IO Bool
notFetchedRecently conn = do
    liftIO $ runRedis conn $ do
        fetched <- exists "nhk_fetched"
        return $ either (const True) not fetched

setFetchedKey :: Connection -> IO ()
setFetchedKey conn = do
  void $ runRedis conn $ do
    _ <- set "nhk_fetched" "true"
    expire "nhk_fetched" 3600
