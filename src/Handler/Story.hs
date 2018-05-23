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
import Text.Blaze.Html (preEscapedToHtml)
import Yesod.Paginator
import Type.Slug

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getStoryR :: Slug -> Handler Html
getStoryR slug = do
  Entity sid story <- runDB $ getBy404 $ UniqueNewsId slug
  defaultLayout $ do
      setTitle "Welcome To Yesod!"
      $(widgetFile "story")
