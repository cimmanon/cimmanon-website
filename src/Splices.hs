{-# LANGUAGE OverloadedStrings #-}

module Splices where

------------------------------------------------------------------------------

import qualified Data.Text as T
import Heist (getParamNode, localHS, AttrSplice)
import Heist.Interpreted
import Heist.SpliceAPI
import Text.Digestive.View (View(..), fieldInputChoiceGroup, absoluteRef, listSubViews, fieldInputText, fieldInputChoice)
import Text.Digestive.Heist (getRefAttributes, digestiveSplices', dfInputSelect)
import Text.Digestive.Heist.Extras
import qualified Text.XmlHtml as X

-- for Session stuff
import Snap.Snaplet (SnapletLens)
import Snap.Snaplet.Session (SessionManager)
import Snap.Snaplet.Heist (SnapletISplice)

import Control.Monad.IO.Class (MonadIO)
import Control.Applicative
--import Data.Functor
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid ((<>), mempty)
import Heist.Splices.Camellia
import Heist.Splices.Camellia.Session

import qualified Model.Component as Component
import qualified Model.Image as Image
import qualified Model.Project as Project
import qualified Model.Tag as Tag

import Data.Time.Format
import System.Locale
import Data.Time.Calendar
import Data.Time.Clock

----------------------------------------------------------------------
-- TODO: move these to a library

nameSplices :: Monad m => T.Text -> Splices (Splice m)
nameSplices x = do
	"name" ## textSplice x
	"lcname" ## textSplice $ T.toLower x


generalSplices :: Monad m => Splices (Splice m)
generalSplices = do
	"archivePath" ## stringSplice Project.archivesDirectory

bindSplices' :: Monad m => Splices (AttrSplice m) -> Splices (Splice m) -> Splice m
bindSplices' attrSplices splices = localHS (bindAttributeSplices attrSplices) $ runChildrenWith splices

{----------------------------------------------------------------------------------------------------{
                                                                      | Session Splices
}----------------------------------------------------------------------------------------------------}

userSessionSplices :: SnapletLens b SessionManager -> Splices (SnapletISplice b)
userSessionSplices sess = do
	"user_id" ## sessionInfoSplice sess "user_id"
	"user_name" ## sessionInfoSplice sess "user_name"
	"user_email" ## sessionInfoSplice sess "user_email"
	"isLoggedIn" ## sessionHasSplice sess "user_id"

{----------------------------------------------------------------------------------------------------{
                                                                      | Digestive Splices
}----------------------------------------------------------------------------------------------------}

customDigestiveSplices :: MonadIO m => View T.Text -> Splices (Splice m)
customDigestiveSplices v = do
	"dfPath" ## return [X.TextNode $ T.intercalate "." $ viewContext v]
	"dfScriptValues" ## dfScriptValues v
	"dfPlainText" ## dfPlainText v
	"dfInputCheckboxMultiple" ## dfInputCheckboxMultiple v

-- this is a very crude splice that generates a script element containing a var that holds an object
dfScriptValues :: Monad m => View T.Text -> Splice m
dfScriptValues v = do
	(ref, _) <- getRefAttributes Nothing
	let
		xs = fieldInputChoiceGroup ref v
		var = T.concat ["var " <> ref <> " = {", vals, "};"]
		vals = T.intercalate ", " $ map (\(x, ys) -> T.concat [x, ": [", tags ys, "]"]) xs
		tags = T.intercalate ", " . map (\(i, name, _) -> "'" <> i <> "'")
	return [X.Element "script" [("type", "text/javascript")] [X.TextNode var]]

{----------------------------------------------------------------------------------------------------{
                                                                      | Project Splices
}----------------------------------------------------------------------------------------------------}

projectSplices :: Monad m => Project.Project -> Splices (Splice m)
projectSplices p = do
	"name" ## textSplice $ Project.name p
	"description" ## markdownSplice $ Project.description p
	"slug" ## textSplice $ Project.slug p
	"url" ## maybeSplice (\x -> runChildrenWith $ "href" ## textSplice x) $ Project.url p
	"featured" ##  showSplice $ Project.featured p

-- TODO: reduce code duplication here
projectComponentSplices :: Monad m => (Project.Project, [(Component.Component, Maybe Image.Image)]) -> Splices (Splice m)
projectComponentSplices (p, cx) = do
	projectSplices p
	"component" ## listToSplice cSplices cx
	"image" ## listToSplice imageSplices $ if hasImages then images else []
	where
		images = mapMaybe snd cx
		hasImages = length images == 1
		cSplices (c, xs) = do
			componentSplices c
			"image" ## if hasImages
				then hideContents
				else maybeSplice (runChildrenWith . imageSplices) xs

projectComponentSplices' :: Monad m => (Project.Project, [(Component.Component, [Image.Image])]) -> Splices (Splice m)
projectComponentSplices' (p, cx) = do
	projectSplices p
	"component" ## listToSplice cSplices cx
	"image" ## listToSplice imageSplices $ if hasImages then images else []
	where
		images = concatMap snd cx
		hasImages = length images == 1
		cSplices (c, xs) = do
			componentSplices c
			"image" ## if hasImages
				then hideContents
				else listToSplice imageSplices xs

{----------------------------------------------------------------------------------------------------{
                                                                      | Component Splices
}----------------------------------------------------------------------------------------------------}

componentSplices :: Monad m => Component.Component -> Splices (Splice m)
componentSplices c = do
	"type" ## textSplice $ Component.typ c
	"description" ## markdownSplice $ Component.description c
	"date" ## dateSplice $ Component.date c
	"public" ## showSplice $ Component.public c
	"featured" ## showSplice $ Component.featured c
	"archived" ## maybeSplice (\x -> ifSplice' $ "/" == T.take 1 x) $ Component.archived c
	"href" ## maybeSplice textSplice $ Component.archived c
	"tag" ## listSplice "name" $ Component.tags c

{----------------------------------------------------------------------------------------------------{
                                                                      | Tag Splices
}----------------------------------------------------------------------------------------------------}

tagCategorySplices :: Monad m => Tag.Category -> Splices (Splice m)
tagCategorySplices c = do
	"name" ## textSplice $ Tag.category c
	"tag" ## listSplice "name" $ Tag.tags c

{----------------------------------------------------------------------------------------------------{
                                                                      | Image Splices
}----------------------------------------------------------------------------------------------------}

imageSplices :: Monad m => Image.Image -> Splices (Splice m)
imageSplices i = do
	"filename" ## textSplice $ Image.filename i
	"width" ## numericSplice $ Image.width i
	"height" ## numericSplice $ Image.height i
	"featured" ## showSplice $ Image.featured i
	"isDefault" ## ifSplice' $ Image.featured i

checkedSplice :: Monad m => Bool -> AttrSplice m
checkedSplice True = const $ return [("checked", "checked")]
checkedSplice _    = mempty

{----------------------------------------------------------------------------------------------------{
                                                                      | Archive Serve
}----------------------------------------------------------------------------------------------------}

dateSplice :: (Monad m, FormatTime t) => t -> Splice m
dateSplice t = dateFormatSplice defaultTimeLocale "%Y-%m-%d" t

archiveServeSplices :: (Monad m) => Splices (Splice m)
archiveServeSplices = do
	"entry" ## listToSplice eSplices entries
	"photo" ## listToSplice pSplices photos
	where
		entries =
			[ (UTCTime (fromGregorian 2000 9  2) 32825, "Cats like baths, too")
			, (UTCTime (fromGregorian 2000 9  8) 48291, "Case of the missing orange juice")
			, (UTCTime (fromGregorian 2000 9  9) 69203, "Just not my week")
			, (UTCTime (fromGregorian 2000 9 14) 59028, "Little black dress")
			, (UTCTime (fromGregorian 2000 9 17) 78937, "Fall is coming")
			]
		eSplices (d, t) = do
			"date" ## dateSplice d
			"title" ## textSplice t

		photos :: [(Int, T.Text)]
		photos =
			[ (1, "Painted Sky")
			, (2, "Boy Chasing Seagull")
			, (3, "Washed Ashore")
			, (4, "Suspended Leaves")
			, (5, "Cave of Tides")
			]
		pSplices (n, t) = do
			"number" ## numericSplice n
			"title" ## textSplice t
