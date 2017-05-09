module Model.Types.Project where

import Control.Applicative
import Data.Text (Text)
import Snap.Snaplet.PostgresqlSimple
import Database.PostgreSQL.Simple.ToField

----------------------------------------------------------------------

data Project = Project
	{ name :: Text
	, description :: Text
	, slug :: Text
	, url :: Maybe Text
	, featured :: Bool
	} deriving (Show, Eq)

instance FromRow Project where
	fromRow = Project <$> field <*> field <*> field <*> field <*> field

instance ToRow Project where
	toRow c =
		[ toField $ name c
		, toField $ description c
		, toField $ slug c
		, toField $ url c
		, toField $ featured c
		]
