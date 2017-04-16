{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances #-}

module Model.Image
	( Image(..)
	, uploadForm
	, updateForm
	, formConfig
	, uploadPolicy
	, partPolicy
	, list
	, add
	, update
	, screenshotDirectory
	) where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>), mappend)
import Snap.Snaplet.PostgresqlSimple

import Data.Either (lefts)
--import Data.Maybe (listToMaybe)
import Data.Text (Text, pack, unpack)
import Data.Vector (fromList)
import Text.Digestive
--import Database.PostgreSQL.Simple.Tuple
import Util.Database

import qualified Codec.Picture as I
import qualified Codec.Picture.Types as I
import Data.Int (Int64)
import Data.List (find)
import Snap.Util.FileUploads
import System.FilePath (takeFileName)
import System.Directory (createDirectoryIfMissing, copyFile, removeFile)
import Text.Digestive.Snap hiding (uploadPolicy, partPolicy)
import Util.Digestive

import Model.Types.Project as P hiding (featured)
import Model.Types.Component as C hiding (featured)
import Model.Types.Image
import Model.Component as C (primaryKey)

----------------------------------------------------------------------

screenshotDirectory :: FilePath
screenshotDirectory = "static/screenshots/"

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}

uploadForm :: Monad m => Form Text m [FilePath]
uploadForm = "file" .: fileMultiple

updateForm :: Monad m => [Image] -> Form Text m (Text, [Text])
updateForm xs = ( , )
	<$> "featured" .: choiceWith choices current
	<*> "delete" .: choiceWithMultiple choices Nothing
	where
		choices = map (toChoice filename filename filename) xs
		current = filename <$> find featured xs

{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

list :: (HasPostgres m) => Project -> Component -> m [Image]
list p c = query [sqlFile|sql/portfolio/images/list.sql|] (C.primaryKey p c)

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
		processFile f = do
			img <- liftIO $ I.readImage f
			case img of
				Left _ -> return $ Left f
				Right i -> storeData f i
		storeData f i = do
			r <- toEither' $ execute [sqlFile|sql/portfolio/images/add.sql|] (P.name p, C.typ c, C.date c, getFileName f, getWidth i, getHeight i)
			case r of
				Left _ -> return $ Left f
				Right _ -> do
					liftIO $ saveFile (filePath p) f
					return $ Right ()

update :: (HasPostgres m, Functor m) => Project -> Component -> (Text, [Text]) -> m (Either Text [Only ()])
update p c (f, d) = do
	liftIO $ mapM (removeFile . mappend (filePath p) . unpack) d
	toEither' $ query [sqlFile|sql/portfolio/images/update.sql|] (P.name p, C.typ c, C.date c, f, fromList d)

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

filePath :: Project -> FilePath
filePath p = screenshotDirectory <> unpack (P.slug p) <> "/"

saveFile :: FilePath -> FilePath -> IO ()
saveFile dir fileName' = do
	createDirectoryIfMissing True dir
	copyFile fileName' $ dir <> getFileName fileName' -- TODO: strip EXIF data

-- filenames uploaded via Snap are prefixed like so:  _snap-c8XbTISLfW2rs
getFileName :: FilePath -> FilePath
getFileName = addDash . splitAt 13 . drop 6 . takeFileName
	where
		addDash (a, b) = a <> ('-' : b)

-- extracting dimensions from JuicyPixels Image type
getWidth :: I.DynamicImage -> Int
getWidth = I.dynamicMap I.imageWidth

getHeight :: I.DynamicImage -> Int
getHeight = I.dynamicMap I.imageHeight
