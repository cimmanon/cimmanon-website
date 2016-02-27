{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Project
	( Project(..)
	, projectForm
	, list
	, listByTag
	, listByYear
	, listByType
	, get
	, getWithComponent
	, adminList
	, add
	, edit
	, years
	, archivesDirectory
	) where

import Control.Applicative
import Control.Exception (catch, IOException)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))
import Snap.Snaplet.PostgresqlSimple

import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack)
import Text.Digestive
import System.Directory (renameDirectory)
import Database.PostgreSQL.Simple.Tuple
import Util.Database

import Model.Types.Project
import Model.Types.Component as C hiding (description, featured)
import Model.Types.Image as I hiding (featured)
import Model.Image as I (screenshotDirectory)

----------------------------------------------------------------------

archivesDirectory :: FilePath
archivesDirectory = "archives/"

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}

projectForm :: Monad m => Maybe Project -> Form Text m Project
projectForm p = Project
	<$> "name" .: notEmpty (text (name <$> p))
	<*> "description" .: notEmpty (text (description <$> p))
	<*> "slug" .: notEmpty (text (slug <$> p))
	<*> "url" .: optionalText (url =<< p)
	<*> "featured" .: bool (featured <$> p)
	where
		notEmpty = check "Cannot be empty" (/= "")

{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

list :: (HasPostgres m, Functor m) => m [(Project, [(Component, Maybe Image)])]
list = join1of3 <$> query_ [sqlFile|sql/portfolio/overview.sql|]

listByTag :: (HasPostgres m, Functor m) => Text -> m [(Project, [(Component, Maybe Image)])]
listByTag x = join1of3 <$> query [sqlFile|sql/portfolio/by_tag.sql|] (Only x)

listByYear :: (HasPostgres m, Functor m) => Int -> m [(Project, [(Component, Maybe Image)])]
listByYear x = join1of3 <$> query [sqlFile|sql/portfolio/by_year.sql|] (Only x)

listByType :: (HasPostgres m, Functor m) => Text -> m [(Project, [(Component, Maybe Image)])]
listByType x = join1of3 <$> query [sqlFile|sql/portfolio/by_type.sql|] (Only x)

get :: (HasPostgres m, Functor m) => Text -> m (Maybe Project)
get s = listToMaybe <$> query "SELECT project, description, slug, url, featured FROM portfolio.projects WHERE slug = ?" (Only s)

getWithComponent :: (HasPostgres m, Functor m) => Text -> Text -> Text -> m (Maybe (Project, Component))
getWithComponent s t d = listToMaybe . map tuple2 <$> query [sqlFile|sql/portfolio/get_with_component.sql|]  (s, t, d)

----------------------------------------------------------------------

adminList :: (HasPostgres m) => m [Project]
adminList = query_ "SELECT project, description, slug, url, featured FROM portfolio.projects ORDER BY project"

add :: (HasPostgres m, Functor m) => Project -> m (Either Text Project)
add p = toEither' $ const p <$> execute "INSERT INTO portfolio.projects (project, description, slug, url, featured) VALUES (?, ?, ?, ?, ?)" (name p, description p, slug p, url p, featured p)

edit :: (HasPostgres m, Functor m) => Project -> Project -> m (Either Text Project)
edit original new = do
	liftIO $ when (oldName /= newName) moveDirectories
	toEither' $ const new <$> execute "UPDATE portfolio.projects SET project = ?, description = ?, slug = ?, url = ?, featured = ? WHERE project = ?" (name new, description new, slug new, url new, featured new, name original)
	where
		oldName = unpack $ slug original
		newName = unpack $ slug new
		catchAndIgnore x = catch x ignore
		ignore :: IOException -> IO ()
		ignore = const (return ())
		moveDirectories = do
			catchAndIgnore $ renameDirectory (I.screenshotDirectory <> oldName) (I.screenshotDirectory <> newName)
			catchAndIgnore $ renameDirectory (archivesDirectory <> oldName) (archivesDirectory <> newName)

----------------------------------------------------------------------

years :: (HasPostgres m, Functor m) => m [Int]
years = map fromOnly <$> query_ "SELECT DISTINCT extract(year FROM date_added) :: Int FROM portfolio.project_components ORDER BY extract(year FROM date_added) :: Int DESC"
