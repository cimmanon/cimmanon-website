{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Tag
	( Category(..)
	, list
	, groupedByCategory
	, groupedByType
	) where

import Control.Applicative
--import Data.Monoid ((<>))
import Snap.Snaplet.PostgresqlSimple

--import Data.Maybe (listToMaybe)
import Data.Text (Text)
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
	fromRow = Category <$> field <*> field

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

list :: (HasPostgres m, Functor m) => m [Category]
list = query_ [sqlFile|sql/portfolio/tag_list.sql|]

groupedByCategory :: HasPostgres m => m [(Text, [Text])]
groupedByCategory = query_ [sqlFile|sql/portfolio/tag_list.sql|]

groupedByType :: HasPostgres m => m [(Text, [Text])]
groupedByType = query_ "SELECT type, array_agg(tag :: text ORDER BY category, tag) FROM portfolio.project_type_tags GROUP BY type ORDER BY type"
