{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Image
	( Image(..)
	, identifyImage
	) where

import Control.Applicative
import Data.Monoid ((<>))
import Snap.Snaplet.PostgresqlSimple

import Data.Maybe (listToMaybe)
import Data.Text (Text, pack)
--import Text.Digestive
import Database.PostgreSQL.Simple.Tuple
import Util.Database

import Data.Char (toLower)
import System.Exit (ExitCode (..))
import System.Process (readProcess, readProcessWithExitCode)

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

data Image = Image
	{ filename :: Text
	, width :: Int
	, height :: Int
	} deriving (Show, Eq)

instance FromRow Image where
	fromRow = Image <$> field <*> field <*> field

instance FromRow (Maybe Image) where
	fromRow = maybeImage <$> field <*> field <*> field

maybeImage :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Image
maybeImage n w h = Image <$> n <*> w <*> h

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}


{----------------------------------------------------------------------------------------------------{
                                                                      | Helper Functions
}----------------------------------------------------------------------------------------------------}

defaultAllowedTypes :: [String]
defaultAllowedTypes = ["png", "gif", "jpg", "jpeg", "svg"]

data UploadError = UnknownType | DisallowedType | EmptyFile deriving (Show)

-- For Image Magick identification
data FileInfo = FileInfo
	{ extension :: String
	, w :: Int
	, h :: Int
	} deriving (Show)

identifyImage :: [String] -> FilePath -> IO (Either UploadError FileInfo)
identifyImage allowedTypes p = do
	result <- readProcessWithExitCode "identify" ["-format", "'%m %w %h'", p] []
	case result of
		(ExitSuccess, r, _) ->
			let
				-- r will contain a string that looks like this: "'SVG 448 103 '\n"
				(ext : width : height : _) = words $ filter (`notElem` "'\n") r
				ext' = map toLower ext
			in if ext' `elem` allowedTypes
				then return $ Right $ FileInfo ext' (read width) (read height)
				else return $ Left DisallowedType
		_ -> return $ Left UnknownType
