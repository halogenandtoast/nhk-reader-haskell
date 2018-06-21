{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Story where

import Import
import Util.Time
import Util.Nhk
import Type.Slug
import Data.Aeson
import Data.Map(elems)


fetchedNewsStory :: Slug -> DB NewsStory
fetchedNewsStory slug = do
    Entity storyId story <- getBy404 $ UniqueNewsId slug
    if newsStoryFetched story
       then return story
       else do
         image <- liftIO $ fetchStoryBase64ImageFromNhk story
         body <- liftIO $ fetchStoryBodyFromNhk story
         maybe (return story) (updateFetched storyId image) body
  where
    updateFetched sid image body = updateGet sid [ NewsStoryBody =. Just body
                                                 , NewsStoryImage =. image
                                                 , NewsStoryFetched =. True
                                                 ]


storiesGroupedByDate :: [Entity NewsStory] -> [(Day, [NewsStory])]
storiesGroupedByDate = storyTuplesByDate . map entityVal

groupStoriesByDate :: [NewsStory] -> [[NewsStory]]
groupStoriesByDate = groupBy ((==) `on` newsStoryPublishedDate)


newsStoryPublishedDate :: NewsStory -> Day
newsStoryPublishedDate = utctDay . newsStoryPublishedAt


storyTuplesByDate :: [NewsStory] -> [(Day, [NewsStory])]
storyTuplesByDate = toTuples . groupStoriesByDate
  where
    toTuples =  mapMaybe toTuple
    toTuple xs@(x:_) = Just (newsStoryPublishedDate x, xs)
    toTuple _ = Nothing


countPublishedToday :: DB Int
countPublishedToday = do
    time <- liftIO getCurrentTime
    count [ NewsStoryPublishedAt >=. beginningOfDay time
          , NewsStoryPublishedAt <=. nextDay time
          ]


importNhkArticle :: NhkArticle -> DB ()
importNhkArticle article = maybe (insert_ story) (const (return ())) =<< getBy (UniqueNewsId nid)
  where
    nid = newsId article
    url = "http://www3.nhk.or.jp/news/easy/" ++ unSlug nid ++ "/" ++ unSlug nid ++ ".html"
    story = NewsStory nid
                      (title article)
                      (titleWithRuby article)
                      Nothing
                      url
                      (parseNhkDate (publishedAt article))
                      False
                      Nothing
                      (toStrict . decodeUtf8 . encode $ originalJson article)


importNhkArticles :: [NhkArticle] -> DB ()
importNhkArticles = foldr ((>>) . importNhkArticle) (return ())


importDateGrouping :: NhkDateGrouping -> DB ()
importDateGrouping (NhkDateGrouping m) = (importNhkArticles . concat . elems) m


importFeed :: NhkFeed  -> DB ()
importFeed (NhkFeed xs) = foldr ((>>) . importDateGrouping) (return ()) xs


refreshStoriesFromNhk :: DB ()
refreshStoriesFromNhk = liftIO getNhkFeed >>= maybe (return ()) importFeed
