{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Project
	( Project(..)
	, list
	, listByTag
	, listByYear
	, get
	, years
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
import qualified Model.Component as C
import qualified Model.Image as I

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

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

list :: (HasPostgres m, Functor m) => m [(Project, [(C.Component, Maybe I.Image)])]
list = join1of3 <$> query_ [sqlFile|sql/portfolio/overview.sql|]

listByTag :: (HasPostgres m, Functor m) => Text -> m [(Project, [(C.Component, Maybe I.Image)])]
listByTag x = join1of3 <$> query [sqlFile|sql/portfolio/by_tag.sql|] (Only x)

listByYear :: (HasPostgres m, Functor m) => Int -> m [(Project, [(C.Component, Maybe I.Image)])]
listByYear x = join1of3 <$> query [sqlFile|sql/portfolio/by_year.sql|] (Only x)

get :: (HasPostgres m, Functor m) => Text -> m (Maybe Project)
get s = listToMaybe <$> query "SELECT project, description, slug, url, public FROM portfolio.projects WHERE slug = ?" (Only s)

----------------------------------------------------------------------

years :: (HasPostgres m, Functor m) => m [Int]
years = map fromOnly <$> query_ "SELECT DISTINCT extract(year FROM date_added) :: Int FROM portfolio.project_components ORDER BY extract(year FROM date_added) :: Int DESC"
