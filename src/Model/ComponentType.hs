{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Model.ComponentType
	( componentTypeForm
	, list
	, admin
	) where

import Control.Applicative
import Data.Int (Int64)
import Data.Text (Text)
import Snap.Snaplet.PostgresqlSimple
import Text.Digestive

import Util.Database
import Util.Digestive

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}

componentTypeForm :: (Monad m, Functor m) => [Text] -> Form Text m ([(Text, Maybe Text)], [Text])
componentTypeForm = itemsForm id nameForm . maybeList

{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

---------------------------------------------------------------------- | Project Components

list :: (HasPostgres m, Functor m) => m [Text]
list = map fromOnly <$> query_ [sqlFile|sql/portfolio/settings/component_types/list.sql|]

admin :: (HasPostgres m, Functor m) => ([(Text, Maybe Text)], [Text]) -> m (Either Text Int64)
admin (keepList, delList) = withTransaction $ manyEithers
	[ toEither' $ executeMany [sqlFile|sql/portfolio/settings/component_types/update.sql|] $ extractRenames keepList
	, toEither' $ executeMany [sqlFile|sql/portfolio/settings/component_types/add.sql|] $ map Only $ extractInserts keepList
	, case delList of
		[] -> return $ Right 0
		_ -> toEither' $ execute [sqlFile|sql/portfolio/settings/component_types/delete.sql|] (Only delList)
	]
