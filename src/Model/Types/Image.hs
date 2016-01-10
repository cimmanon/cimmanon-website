{-# LANGUAGE FlexibleInstances #-}

module Model.Types.Image where

import Control.Applicative
import Data.Text (Text)
import Snap.Snaplet.PostgresqlSimple

----------------------------------------------------------------------

data Image = Image
	{ filename :: Text
	, width :: Int
	, height :: Int
	} deriving (Show, Eq)

instance FromRow Image where
	fromRow = Image <$> field <*> field <*> field

instance FromRow (Maybe Image) where
	fromRow = maybeImage <$> field <*> field <*> field

maybeImage :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Image
maybeImage n w h = Image <$> n <*> w <*> h
