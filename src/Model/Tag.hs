{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Tag
	( Category(..)
	, list
	, listByType
	) where

import Control.Applicative
--import Data.Monoid ((<>))
import Snap.Snaplet.PostgresqlSimple

--import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Vector (toList)
--import Text.Digestive
--import Database.PostgreSQL.Simple.Tuple
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

listByType :: (HasPostgres m, Functor m) => Text -> m [Text]
listByType c = map fromOnly <$> query "SELECT tag FROM portfolio.project_type_tags WHERE type = ?" (Only c)
