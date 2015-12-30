{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Component
	( Component(..)
	, list
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

import Model.Image as I

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

data Component = Component
	{ component :: Text
	, description :: Text
	, date :: Day
	, tags :: [Text]
	} deriving (Show, Eq)

instance FromRow Component where
	fromRow = Component <$> field <*> field <*> field <*> (toList <$> field)

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

list :: (HasPostgres m, Functor m) => Text -> m [(Component, [I.Image])]
list project = ojoin1of2 <$> query [sqlFile|sql/portfolio/components.sql|] (Only project)
