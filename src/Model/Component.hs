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
import Control.Arrow (second)
import Data.Monoid ((<>))
import Snap.Snaplet.PostgresqlSimple

import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (find)
import Data.Text (Text, pack)
import Data.Time.Calendar
import Data.Vector (fromList)
import Text.Digestive
import Database.PostgreSQL.Simple.Tuple
import Util.Database
import Util.Form

import Model.Types.Project as P hiding (description, featured)
import Model.Types.Component as C
import Model.Types.Image as I hiding (featured)
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
	tags' <- T.groupedByType
	let
		typeChoices = map (\(x, _) -> (x, (x, x))) tags'
	return $ Component
		<$> "type" .: choiceWith typeChoices (either (const Nothing) (Just . typ) c)
		<*> "description" .: notEmpty (text (description <$> toMaybe c))
		<*> "date" .: eitherDisable (dateFormlet "%F" (date <$> toMaybe c))
		<*> "public" .: bool (either (const (Just True)) (Just . public) c)
		<*> "featured" .: bool (either (const (Just True)) (Just . featured) c)
		<*> "archived" .: optionalText (archived =<< toMaybe c)
		<*> "tags" .: tagsForm tags' (either (const []) tags c) (either (Just . id) (Just . typ) c)
	where
		notEmpty = check "Cannot be empty" (/= "")
		toMaybe = either (const Nothing) Just
		toChoice = map (second (map (\x -> (x, (x, x)))))
		-- editing a component should not allow you to edit the date
		eitherDisable = either (const id) (const disable) c

tagsForm :: (Monad m, Functor m) => [(Text, [Text])] -> [Text] -> Maybe Text -> Form Text m [Text]
tagsForm haystack needle group = fmap snd $ ( , )
	<$> "allTags" .: groupedChoiceWith (toChoice haystack) Nothing
	<*> "tags" .: listOfText (fromMaybe defaultTags currentTags) needle
	where
		-- TODO: make this safe
		defaultTags = snd $ head haystack
		currentTags = snd <$> find ((==) group' . fst) haystack
		group' = fromMaybe (fst $ head haystack) group
		toChoice = map (\(x, xs) -> (x, map (\y -> (y, (y, y))) xs))

----------------------------------------------------------------------

componentTypeForm :: (Monad m, Functor m) => Form Text m (Maybe Text, [Text])
componentTypeForm = ( , )
	<$> "add" .: optionalText Nothing
	<*> pure []

{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

---------------------------------------------------------------------- | Project Components

list :: (HasPostgres m, Functor m) => Project -> m [(Component, [I.Image])]
list p = ojoin1of2 <$> query [sqlFile|sql/portfolio/components.sql|] (Only $ P.name p)

get :: (HasPostgres m, Functor m) => Project -> Text -> Text -> m (Maybe Component)
get p c d = listToMaybe <$> query [sqlFile|sql/portfolio/component.sql|] (P.name p, c, d)

----------------------------------------------------------------------

adminList :: HasPostgres m => Project -> m [Component]
adminList p = query "SELECT type, description, date_added, public, featured, archived, array[] :: text[] AS tags FROM portfolio.project_components WHERE project = ? ORDER BY date_added" (Only $ P.name p)

add :: (HasPostgres m, Functor m) => Project -> Component -> m (Either Text Component)
add p c = toEither' $ const c <$> q
	where
		-- query split off here rather than write a one-liner to avoid ambiguity when the type is discarded above
		q :: HasPostgres m => m [Only ()]
		q = query "SELECT portfolio.add_component((?, ?, ?, ?, ?, ?, ?) :: portfolio.PROJECT_COMPONENTS, ?)" (P.name p, typ c, date c, description c, public c, featured c, archived c, fromList $ tags c)

-- TODO: either allow editing of type and date of the project, or disable the form controls for it
edit :: (HasPostgres m, Functor m) => Project -> Component -> m (Either Text [Only ()])
edit p c = toEither' $ query "SELECT portfolio.edit_component((?, ?, ?, ?, ?, ?, ?) :: portfolio.PROJECT_COMPONENTS, ?)" (P.name p, typ c, date c, description c, public c, featured c, archived c, fromList $ tags c)

--------------------------------------------------------------------- | Component Types

types :: (HasPostgres m, Functor m) => m [Text]
types = map fromOnly <$> query_ "SELECT type :: TEXT FROM portfolio.project_types ORDER BY type"
