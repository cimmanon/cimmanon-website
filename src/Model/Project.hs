{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Project
	( Project(..)
	, Component(..)
	, list
	, get
	, components
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

import Control.Monad.Trans (liftIO)
import Model.Image as I

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

list :: (HasPostgres m, Functor m) => m [(Project, [(Component, Maybe I.Image)])]
list = join1of3 <$> query_ [sqlFile|sql/portfolio/overview.sql|]

get :: (HasPostgres m, Functor m) => Text -> m (Maybe Project)
get s = listToMaybe <$> query "SELECT name, description, slug, url, public FROM portfolio.projects WHERE slug = ?" (Only s)

components :: (HasPostgres m, Functor m) => Text -> m [(Component, [I.Image])]
components p = ojoin1of2 <$> query [sqlFile|sql/portfolio/components.sql|] (Only p)
