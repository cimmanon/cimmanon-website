{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Component
	( Component(..)
	, componentForm
	, path
	, list
	, get
	, adminList
	, add
	, edit
	, types
	) where

import Control.Applicative
import Data.Monoid ((<>))
import Snap.Snaplet.PostgresqlSimple

import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack, replace, toLower)
import Data.Time.Calendar
import Data.Vector (toList, fromList)
import Text.Digestive
import Database.PostgreSQL.Simple.Tuple
import Util.Database
import Util.Form

import qualified Model.Image as I
import qualified Model.Tag as T

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

data Component = Component
	{ component :: Text
	, description :: Text
	, date :: Day
	, public :: Bool
	, archived :: Bool
	, tags :: [Text]
	} deriving (Show, Eq)

instance FromRow Component where
	fromRow = Component <$> field <*> field <*> field <*> field <*> field <*> (toList <$> field)

path :: Component -> Text
path c = component c <> "/" <> (pack $ show $ date c)

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}

componentForm :: (HasPostgres m, Monad m, Functor m) => Either Text Component -> Form Text m Component
componentForm c = monadic $ do
	tags' <- T.listByComponent $ either id component c
	return $ Component
		<$> "component" .: disable (text (either Just (Just . component) c))
		<*> "description" .: notEmpty (text (description <$> toMaybe c))
		<*> "date" .: eitherDisable (dateFormlet "%F" (date <$> toMaybe c))
		<*> "public" .: bool (either (const (Just True)) (Just . public) c)
		<*> "archived" .: bool (archived <$> toMaybe c)
		<*> "tags" .: listOfText tags' (either (const []) tags c)
	where
		notEmpty = check "Cannot be empty" (/= "")
		toMaybe = either (const Nothing) Just
		-- editing a component should not allow you to edit the date
		eitherDisable = either (const id) (const disable) c

{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

list :: (HasPostgres m, Functor m) => Text -> m [(Component, [I.Image])]
list project = ojoin1of2 <$> query [sqlFile|sql/portfolio/components.sql|] (Only project)

get :: (HasPostgres m, Functor m) => Text -> Text -> Text -> m (Maybe Component)
get p c d = listToMaybe <$> query [sqlFile|sql/portfolio/component.sql|] (p, c, d)

----------------------------------------------------------------------

adminList :: HasPostgres m => Text -> m [Component]
adminList project = query "SELECT component, description, date_added, public, archived, array[] :: text[] AS tags FROM portfolio.project_components WHERE project = ? ORDER BY date_added" (Only project)

add :: (HasPostgres m, Functor m) => Text -> Component -> m (Either Text Component)
add project c = toEither' $ (const c) <$> q
	where
		-- query split off here rather than write a one-liner to avoid ambiguity when the type is discarded above
		q :: HasPostgres m => m [Only ()]
		q = query "SELECT portfolio.add_component((?, ?, ?, ?, ?, ?) :: portfolio.PROJECT_COMPONENTS, ?)" (project, component c, date c, description c, public c, archived c, fromList $ tags c)

edit :: (HasPostgres m, Functor m) => Text -> Component -> m (Either Text [(Only ())])
edit project c = toEither' $ query "SELECT portfolio.edit_component((?, ?, ?, ?, ?, ?) :: portfolio.PROJECT_COMPONENTS, ?)" (project, component c, date c, description c, public c, archived c, fromList $ tags c)

----------------------------------------------------------------------

types :: (HasPostgres m, Functor m) => m [Text]
types = map fromOnly <$> query_ "SELECT component FROM portfolio.components ORDER BY component"
