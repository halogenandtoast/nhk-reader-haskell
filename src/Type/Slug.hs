{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.Slug where

import Data.Text
import Database.Persist.Sql

newtype Slug = Slug { unSlug :: Text } deriving (PersistField, PersistFieldSql)
