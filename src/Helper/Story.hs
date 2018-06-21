{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Helper.Story where

import Import
import Hledger.Utils.Regex
import qualified Data.Text as DT

newsStoryTitleWithBr :: NewsStory -> Text
newsStoryTitleWithBr = pack . regexReplace "\12288" "<br>" . unpack . newsStoryTitleWithRuby

sanitizedBody :: NewsStory -> Maybe Text
sanitizedBody newsStory = DT.splitOn "</div>" <$> newsStoryBody newsStory >>= safeHead
  where
    safeHead (x:_) = Just x
    safeHead _ = Nothing
