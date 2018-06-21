{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Util.Nhk where

import Import
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B64
import Network.HTTP.Conduit (simpleHttp)
import Control.Lens hiding (children, element)
import Text.Taggy.Renderer ( render )
import Text.Taggy.Lens
import Type.Slug
import Data.Aeson
import Data.Aeson.Types
import Util.Time
import Hledger.Utils.Regex

data NhkData = NhkData { newsWebImageUri :: Text } deriving (Show)
data NhkArticle = NhkArticle { newsId :: Slug
                             , title :: Text
                             , titleWithRuby :: Text
                             , publishedAt :: Text
                             , originalJson :: Object
                             } deriving (Show)

data NhkDateGrouping = NhkDateGrouping (Map String [NhkArticle])
  deriving (Show, Generic)

data NhkFeed = NhkFeed [NhkDateGrouping]
  deriving (Show, Generic)


instance FromJSON NhkData where
    parseJSON (Object v) = NhkData <$> (v .: "news_web_image_uri")
    parseJSON invalid = typeMismatch "NhkData" invalid

instance FromJSON NhkArticle where
  parseJSON (Object v) =
    NhkArticle <$>
      (v .: "news_id") <*>
      (v .: "title") <*>
      (v .: "title_with_ruby") <*>
      (v .: "news_prearranged_time") <*>
      pure v
  parseJSON invalid = typeMismatch "NhkArticle" invalid

instance FromJSON NhkDateGrouping
instance FromJSON NhkFeed


-- We have to drop the first 3 bytes due to the BOM marker
getNhkResource :: String -> IO B.ByteString
getNhkResource = (B.drop 3 <$>) . simpleHttp


getNhkFeed :: IO (Maybe NhkFeed)
getNhkFeed = do
    ts <- timestamp <$> getCurrentTime
    decode <$> getNhkResource ("http://www3.nhk.or.jp/news/easy/news-list.json?_=" ++ show ts)


fetchStoryBodyFromNhk :: NewsStory -> IO (Maybe Text)
fetchStoryBodyFromNhk story = do
    response <- decodeUtf8 <$> getNhkResource url
    return $ renderWithDictionaryLinks <$> response ^? newsArticle
  where
    newsArticle = html . allAttributed(ix "id" . only "newsarticle")
    url = (unpack . newsStoryUrl) story
    renderWithDictionaryLinks = lazyTextFun replaceDictionaryLinks . render
    replaceDictionaryLinks =  regexReplace "</a>" "</span>" . regexReplace "<a[^>]*>" "<span class=\"lookup\">"
    lazyTextFun f = pack . f . unpack . toStrict


fetchStoryBase64ImageFromNhk :: NewsStory -> IO (Maybe Text)
fetchStoryBase64ImageFromNhk n = do
    let j = decode . fromStrict . encodeUtf8 . newsStoryData $ n
    case j of
         Nothing -> return Nothing
         Just x -> do
           request <- try $ simpleHttp (unpack $ newsWebImageUri x) :: IO (Either HttpException B.ByteString)
           return $ either (const Nothing) (Just . toStrict . decodeUtf8 . B64.encode) request
