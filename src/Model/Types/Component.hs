module Model.Types.Component where

import Control.Applicative
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Vector (toList)
import Snap.Snaplet.PostgresqlSimple

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
	fromRow = Component <$> field <*> field <*> field <*> field <*> field <*> field <*> (toList <$> field)
