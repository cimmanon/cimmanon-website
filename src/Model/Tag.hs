{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Tag
	( Category(..)
	, list
	, listByComponent
	) where

import Control.Applicative
import Data.Monoid ((<>))
import Snap.Snaplet.PostgresqlSimple

import Data.Maybe (listToMaybe)
import Data.Text (Text, pack, replace, toLower)
import Data.Time.Calendar
import Data.Vector (toList)
--import Text.Digestive
import Database.PostgreSQL.Simple.Tuple
import Util.Database

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

data Category = Category
	{ category :: Text
	, tags :: [Text]
	} deriving (Show, Eq)

instance FromRow Category where
	fromRow = Category <$> field <*> (toList <$> field)

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

list :: (HasPostgres m, Functor m) => m [Category]
list = query_ [sqlFile|sql/portfolio/tag_list.sql|]

listByComponent :: (HasPostgres m, Functor m) => Text -> m [Text]
listByComponent c = map fromOnly <$> query "SELECT tag FROM portfolio.component_tags WHERE component = ?" (Only c)
