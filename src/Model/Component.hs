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
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (find)
import Data.Text (Text, pack)
import Data.Time.Calendar
import Text.Digestive
import Database.PostgreSQL.Simple.Tuple
import Util.Database
import Util.Digestive

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
		typeChoices = map (toChoice id id id . fst) tags'
	return $ Component
		<$> eitherDisable ("type" .: choiceWith typeChoices (either (const Nothing) (Just . typ) c))
		<*> eitherDisable ("date" .: dateFormlet "%F" (date <$> toMaybe c))
		<*> "description" .: notEmpty (text (description <$> toMaybe c))
		<*> "public" .: bool (either (const (Just True)) (Just . public) c)
		<*> "featured" .: bool (either (const (Just True)) (Just . featured) c)
		<*> "archived" .: optionalText (archived =<< toMaybe c)
		<*> "tags" .: tagsForm tags' (either (const []) tags c) (either (Just) (Just . typ) c)
	where
		notEmpty = check "Cannot be empty" (/= "")
		toMaybe = either (const Nothing) Just
		-- editing a component should not allow you to edit the date
		eitherDisable = either (const id) (const disable) c

tagsForm :: (Monad m, Functor m) => [(Text, [Text])] -> [Text] -> Maybe Text -> Form Text m [Text]
tagsForm haystack needle group = fmap snd $ ( , )
	<$> "allTags" .: groupedChoiceWith (map toGroupedChoice haystack) Nothing
	<*> "tags" .: choiceWithMultiple (map toChoice' $ fromMaybe defaultTags currentTags) (Just needle)
	where
		-- TODO: make this safe
		defaultTags = snd $ head haystack
		currentTags = snd <$> find ((==) group' . fst) haystack
		group' = fromMaybe (fst $ head haystack) group
		toChoice' = toChoice id id id
		toGroupedChoice (x, xs) = (x, map toChoice' xs)

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
list p = ojoin1of2 <$> query [sqlFile|sql/portfolio/components/list.sql|] (Only $ P.name p)

get :: (HasPostgres m, Functor m) => Project -> Text -> Text -> m (Maybe Component)
get p c d = listToMaybe <$> query [sqlFile|sql/portfolio/components/get.sql|] (P.name p, c, d)

----------------------------------------------------------------------

adminList :: (HasPostgres m, Functor m) => Project -> m [(Component, Maybe I.Image)]
adminList p = map tuple2 <$> query [sqlFile|sql/portfolio/components/list_admin.sql|] (Only $ P.name p)

add :: (HasPostgres m, Functor m) => Project -> Component -> m (Either Text Component)
add p c = toEither' $ const c <$> execute [sqlFile|sql/portfolio/components/add.sql|] (Only (P.name p) :. c)

-- TODO: either allow editing of type and date of the project, or disable the form controls for it
edit :: (HasPostgres m, Functor m) => Project -> Component -> m (Either Text Int64)
edit p c = toEither' $ execute [sqlFile|sql/portfolio/components/update.sql|] (Only (P.name p) :. c :. primaryKey p c)

--------------------------------------------------------------------- | Component Types

types :: (HasPostgres m, Functor m) => m [Text]
types = map fromOnly <$> query_ "SELECT type :: TEXT FROM portfolio.project_types ORDER BY type"
