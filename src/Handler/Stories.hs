{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Stories where

import Import
import Util.Redis
import Model.Story
import Database.Persist
import qualified Database.Redis as R
import Text.Julius (RawJS (..))
import Text.Blaze.Html (preEscapedToHtml)
import Yesod.Paginator

getStoriesR :: Handler Html
getStoriesR = do
    App {..} <- getYesod
    (groups, widget) <- runDB $ do
        shouldFetch <- liftIO $ notFetchedRecently appRedisConn
        when shouldFetch ((liftIO $ setFetchedKey appRedisConn) >> refreshStoriesFromNhk)
        (stories, widget) <- selectPaginated 15 [] [Desc NewsStoryPublishedAt]
        return (storiesGroupedByDate stories, widget)
    defaultLayout $ do
        setTitle "NhkReader"
        $(widgetFile "stories")
