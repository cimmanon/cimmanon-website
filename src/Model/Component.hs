{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Component
	( Component(..)
	, componentForm
	, path
	, primaryKey
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

import Model.Types.Project as P hiding (description)
import Model.Types.Component as C
import Model.Types.Image as I
import qualified Model.Tag as T

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

path :: Component -> Text
path c = typ c <> "/" <> pack (show $ date c)

primaryKey :: Project -> Component -> (Text, Text, Day)
primaryKey p c = (P.name p, C.typ c, C.date c)

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}

componentForm :: (HasPostgres m, Monad m, Functor m) => Either Text Component -> Form Text m Component
componentForm c = monadic $ do
	tags' <- T.listByType $ either id typ c
	return $ Component
		<$> "type" .: disable (text (either Just (Just . typ) c))
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

list :: (HasPostgres m, Functor m) => Project -> m [(Component, [I.Image])]
list p = ojoin1of2 <$> query [sqlFile|sql/portfolio/components.sql|] (Only $ P.name p)

get :: (HasPostgres m, Functor m) => Project -> Text -> Text -> m (Maybe Component)
get p c d = listToMaybe <$> query [sqlFile|sql/portfolio/component.sql|] (P.name p, c, d)

----------------------------------------------------------------------

adminList :: HasPostgres m => Project -> m [Component]
adminList p = query "SELECT type, description, date_added, public, archived, array[] :: text[] AS tags FROM portfolio.project_components WHERE project = ? ORDER BY date_added" (Only $ P.name p)

add :: (HasPostgres m, Functor m) => Project -> Component -> m (Either Text Component)
add p c = toEither' $ const c <$> q
	where
		-- query split off here rather than write a one-liner to avoid ambiguity when the type is discarded above
		q :: HasPostgres m => m [Only ()]
		q = query "SELECT portfolio.add_component((?, ?, ?, ?, ?, ?) :: portfolio.PROJECT_COMPONENTS, ?)" (P.name p, typ c, date c, description c, public c, archived c, fromList $ tags c)

edit :: (HasPostgres m, Functor m) => Project -> Component -> m (Either Text [Only ()])
edit p c = toEither' $ query "SELECT portfolio.edit_component((?, ?, ?, ?, ?, ?) :: portfolio.PROJECT_COMPONENTS, ?)" (P.name p, typ c, date c, description c, public c, archived c, fromList $ tags c)

----------------------------------------------------------------------

types :: (HasPostgres m, Functor m) => m [Text]
types = map fromOnly <$> query_ "SELECT type :: TEXT FROM portfolio.project_types ORDER BY type"
