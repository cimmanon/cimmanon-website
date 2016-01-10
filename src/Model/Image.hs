{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Image
	( Image(..)
	, imageForm
	, identifyImage
	, formConfig
	, uploadPolicy
	, partPolicy
	, list
	, add
	) where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))
import Snap.Snaplet.PostgresqlSimple

import Data.Either (lefts)
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack, unpack)
import Text.Digestive
import Database.PostgreSQL.Simple.Tuple
import Util.Database

import Data.Char (toLower)
import Data.Int (Int64)
import Snap.Util.FileUploads
import System.FilePath (takeFileName)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Directory (createDirectoryIfMissing, copyFile)
import Text.Digestive.Snap hiding (uploadPolicy, partPolicy)

import Model.Types.Project as P
import Model.Types.Component as C
import Model.Types.Image as I
import Model.Component as C (primaryKey)

----------------------------------------------------------------------

screenshotDirectory :: FilePath
screenshotDirectory = "static/images/screenshots/"

allowedTypes :: [String]
allowedTypes = ["png", "gif", "jpg", "jpeg", "svg"]

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}

imageForm :: Monad m => Form Text m [FilePath]
imageForm = "file" .: fileMultiple

{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

list :: (HasPostgres m) => Project -> Component -> m [Image]
list p c = query "SELECT filename, width, height FROM portfolio.project_images WHERE project = ? AND component = ? AND date_added = ?" (C.primaryKey p c)

add :: (HasPostgres m, Functor m) => Project -> Component -> [FilePath] -> m (Either Text ())
add p c xs = do
	res <- mapM processFile xs
	let
		failures = lefts res
	if null failures
		then return $ Right ()
		else do
			liftIO $ print failures
			return $ Left $ "Images failed to upload: " <> pack (show $ length failures)
	where
		destDir = screenshotDirectory <> unpack (P.slug p) <> "/"
		processFile f = do
			info <- liftIO $ identifyImage allowedTypes f
			case info of
				Left _ -> return $ Left f
				Right i -> storeData f i
		storeData f i = do
			r <- toEither' $ execute [sqlFile|sql/portfolio/insert_image.sql|] (P.name p, C.component c, C.date c, getFileName f, w i, h i)
			case r of
				Left _ -> return $ Left f
				Right _ -> do
					liftIO $ saveFile destDir f
					return $ Right ()

saveFile :: FilePath -> FilePath -> IO ()
saveFile dir file = do
	createDirectoryIfMissing True dir
	copyFile file $ dir <> getFileName file -- TODO: strip EXIF data

-- filenames uploaded via Snap are prefixed like so:  _snap-c8XbTISLfW2rs
getFileName :: FilePath -> FilePath
getFileName = drop 6 . takeFileName

{----------------------------------------------------------------------------------------------------{
                                                                      | Upload Policy
}----------------------------------------------------------------------------------------------------}

formConfig :: SnapFormConfig
formConfig = SnapFormConfig Nothing Nothing uploadPolicy (const $ allowWithMaximumSize (128 * 1024))

uploadPolicy :: UploadPolicy
uploadPolicy = setMaximumNumberOfFormInputs 50 defaultUploadPolicy -- NOTE: not sure how many fields to allow for, 50 seems ok

partPolicy :: Int64 -> PartInfo -> PartUploadPolicy
partPolicy maxSize p =
	case partFileName p of
		--Just x | x /= "" -> allowWithMaximumSize maxSize -- an empty string for the file name means no file was chosen when the form was uploaded?
		Just _ -> allowWithMaximumSize maxSize
		_ -> disallow

{----------------------------------------------------------------------------------------------------{
                                                                      | Helper Functions
}----------------------------------------------------------------------------------------------------}

data UploadError = UnknownType | DisallowedType | EmptyFile deriving (Show)

-- For Image Magick identification
data FileInfo = FileInfo
	{ extension :: String
	, w :: Int
	, h :: Int
	} deriving (Show)

identifyImage :: [String] -> FilePath -> IO (Either UploadError FileInfo)
identifyImage allowedTypes p = do
	result <- readProcessWithExitCode "identify" ["-format", "%m %w %h", p] []
	case result of
		(ExitSuccess, r, _) ->
			let
				-- r will contain a string that looks like this: "SVG 448 103\n"
				(ext : width : height : _) = words r
				ext' = map toLower ext
			in return $ if ext' `elem` allowedTypes
				then Right $ FileInfo ext' (read width) (read height)
				else Left DisallowedType
		_ -> return $ Left UnknownType
