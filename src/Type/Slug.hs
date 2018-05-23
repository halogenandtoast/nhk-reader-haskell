{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.Slug where

import ClassyPrelude.Yesod
import Database.Persist.Sql

newtype Slug = Slug { unSlug :: Text } deriving (Eq, Show, Read, PathPiece, PersistField, PersistFieldSql)
