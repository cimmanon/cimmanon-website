module Model.Types.Project where

import Control.Applicative
import Data.Text (Text)
import Snap.Snaplet.PostgresqlSimple

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
