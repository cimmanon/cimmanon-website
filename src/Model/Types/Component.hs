module Model.Types.Component where

import Control.Applicative
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Snap.Snaplet.PostgresqlSimple
import Database.PostgreSQL.Simple.ToField

import Util.Database

----------------------------------------------------------------------

data Component = Component
	{ typ :: Text
	, description :: Text
	, date :: Day
	, public :: Bool
	, featured :: Bool
	, archived :: Maybe Text
	, tags :: [Text]
	} deriving (Show, Eq)

instance FromRow Component where
	fromRow = Component <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Component where
	toRow c =
		[ toField $ typ c
		, toField $ date c
		, toField $ description c
		, toField $ public c
		, toField $ featured c
		, toField $ archived c
		, toField $ tags c
		]
