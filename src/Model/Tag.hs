{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Tag
	( Tag(..)
	, tagsForm
	, list
	, adminList
	, admin
	, groupedByCategory
	, groupedByType
	) where

import Control.Arrow (first)
import Control.Applicative
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Tuple
import Snap.Snaplet.PostgresqlSimple
import Text.Digestive

import Util.Database
import Util.Digestive

import qualified Model.TagCategory as TagCategory (list)

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

data Tag = Tag
	{ componentType :: Text
	, tag :: Text
	, category :: Text
	} deriving (Show, Eq)

instance FromRow Tag where
	fromRow = Tag <$> field <*> field <*> field

instance ToRow Tag where
	toRow t =
		[ toField $ componentType t
		, toField $ tag t
		, toField $ category t
		]

primaryKey :: Tag -> (Text, Text)
primaryKey t = (componentType t, tag t)

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}

tagsForm :: (Monad m, Functor m, HasPostgres m) => [(Text, [(Text, Text)])] -> Form Text m ([(Tag, Maybe (Text, Text))], [(Text, Text)])
tagsForm xs = monadic $ do
	categories <- TagCategory.list
	return $ validate combineResults $ listOf (typeForm categories) $ Just xs
	where
		combineResults ys = Success (concatMap fst ys, concatMap snd ys)

typeForm :: (Monad m, Functor m) => [Text] -> Maybe (Text, [(Text, Text)]) -> Form Text m ([(Tag, Maybe (Text, Text))], [(Text, Text)])
typeForm categories t = flip const
	<$> "component_type" .: text (fst <$> t)
	<*> "tags" .: itemsForm pri (tagForm categories (fst <$> t)) (map (uncurry toTag) . snd <$> t)
	where
		pri x = (componentType x, tag x)
		toTag = Tag (maybe "" fst t)

tagForm :: (Monad m, Functor m) => [Text] -> Maybe Text -> Maybe Tag -> Form Text m Tag
tagForm categories typ t = Tag
	<$> "component_type" .: text typ
	<*> "tag" .: text (tag <$> t)
	<*> "category" .: choiceWith (map toChoice categories) (category <$> t)
	where
		toChoice x = (x, (x, x))

{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

list :: (HasPostgres m, Functor m) => m [Tag]
list = query_ [sqlFile|sql/portfolio/settings/tags/list.sql|]

adminList :: (HasPostgres m, Functor m) => m [(Text, [(Text, Text)])]
adminList = map (first fromOnly) . ojoin1of2 <$> query_ [sqlFile|sql/portfolio/settings/tags/list_admin.sql|]

admin :: (HasPostgres m, Functor m) => ([(Tag, Maybe (Text, Text))], [(Text, Text)]) -> m (Either Text Int64)
admin (keepList, delList) = withTransaction $ manyEithers
	[ toEither' $ executeMany [sqlFile|sql/portfolio/settings/tags/update.sql|] $ map (uncurry (:.)) $ extractUpdates keepList
	, toEither' $ executeMany [sqlFile|sql/portfolio/settings/tags/add.sql|] $ extractInserts keepList
	, case delList of
		[] -> return $ Right 0
		_ -> toEither' $ executeMany [sqlFile|sql/portfolio/settings/tags/delete.sql|] delList
	]

groupedByCategory :: HasPostgres m => m [(Text, [Text])]
groupedByCategory = query_ [sqlFile|sql/portfolio/settings/tags/grouped_by_category.sql|]

groupedByType :: HasPostgres m => m [(Text, [Text])]
groupedByType = query_ [sqlFile|sql/portfolio/settings/tags/grouped_by_type.sql|]
