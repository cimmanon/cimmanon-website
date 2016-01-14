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
	, featured :: Bool
	} deriving (Show, Eq)

instance FromRow Image where
	fromRow = Image <$> field <*> field <*> field <*> field

instance FromRow (Maybe Image) where
	fromRow = maybeImage <$> field <*> field <*> field <*> field

maybeImage :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Image
maybeImage n w h f = Image <$> n <*> w <*> h <*> f
