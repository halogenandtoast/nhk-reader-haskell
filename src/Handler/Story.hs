{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Story where

import Import
import Database.Persist
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Text.Blaze.Html (preEscapedToHtml, toMarkup)
import Yesod.Paginator
import Type.Slug
import Model.Story
import Helper.Story

getStoryR :: Slug -> Handler Html
getStoryR slug = do
    story <- runDB $ fetchedNewsStory slug
    defaultLayout $ do
      setTitleI (newsStoryTitle story)
      $(widgetFile "story")
