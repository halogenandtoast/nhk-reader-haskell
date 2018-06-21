{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.Slug where

import ClassyPrelude.Yesod
import Database.Persist.Sql
import qualified Data.Aeson as A

newtype Slug = Slug { unSlug :: Text } deriving (Eq, Show, Read, PathPiece, PersistField, PersistFieldSql)

instance FromJSON Slug where
  parseJSON = A.withText "Slug" $ \txt -> return (Slug txt)
