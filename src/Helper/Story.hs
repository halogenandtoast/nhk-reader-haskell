{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Helper.Story where

import Import
import Hledger.Utils.Regex

newsStoryTitleWithBr :: NewsStory -> Text
newsStoryTitleWithBr = pack . regexReplace "\12288" "<br>" . unpack . newsStoryTitleWithRuby
