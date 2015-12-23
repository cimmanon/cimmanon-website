{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Project
	( Project(..)
	, Component(..)
	, list
	) where

import Control.Applicative
import Data.Monoid ((<>))
import Snap.Snaplet.PostgresqlSimple

import Data.Text (Text, pack, replace, toLower)
import Data.Time.Calendar
import Data.Vector (toList)
--import Text.Digestive
import Database.PostgreSQL.Simple.Tuple
import Util.Database

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

data Project = Project
	{ name :: Text
	, description :: Text
	, slug :: Text
	, url :: Maybe Text
	, public :: Bool
	} deriving (Show, Eq)

instance FromRow Project where
	fromRow = Project <$> field <*> field <*> field <*> field <*> field

{-
maybeExternalLink :: Maybe Text -> Maybe Text -> Maybe ExternalLink
maybeExternalLink (Just a) (Just b) = Just $ ExternalLink a b
maybeExternalLink _ _ = Nothing
-}

----------------------------------------------------------------------

data Component = Component
	{ component :: Text
	, desc :: Text
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

list :: (HasPostgres m, Functor m) => m [(Project, [Component])]
list = join1of2 <$> query_ [sqlFile|sql/portfolio/overview.sql|]
{-
listByColor :: (HasPostgres m) => m [Project]
listByColor = query_ [sqlFile|sql/projects/list_by_color.sql|]

get :: (HasPostgres m, Functor m) => Text -> m (Maybe (Project, [ExternalLink]))
get s = listToMaybe . ojoin1of2 <$> query [sqlFile|sql/projects/single.sql|] (Only s)

builds :: (HasPostgres m, Functor m) => Project -> m [([Int], Int)]
builds c = map (first toList) <$> query [sqlFile|sql/projects/builds.sql|] (name c, style c)
-}