{-# LANGUAGE NoImplicitPrelude #-}
module Util.Time where

import Import
import Data.Time.Calendar
import Data.Time.Clock.POSIX
import Data.Time.Format

beginningOfDay :: UTCTime -> UTCTime
beginningOfDay = flip UTCTime 0 . utctDay

nextDay :: UTCTime -> UTCTime
nextDay = flip UTCTime 0 . addDays 1 . utctDay

timestamp :: UTCTime -> Int
timestamp = floor . (* 1000) . utcTimeToPOSIXSeconds

parseNhkDate :: Text -> UTCTime
parseNhkDate s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (unpack s)
