{-# LANGUAGE OverloadedStrings #-}

module Splices where

------------------------------------------------------------------------------

import Data.Map.Syntax ((##))
import qualified Data.Text as T
import Heist (localHS, AttrSplice, Splices)
import Heist.Interpreted
--import qualified Text.XmlHtml as X

--import Data.Functor
import Data.Maybe (mapMaybe)
import Data.Monoid (mempty)
import Heist.Splices.Camellia

import qualified Model.Component as Component
import qualified Model.Image as Image
import qualified Model.Project as Project
import qualified Model.Tag as Tag

import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Clock

----------------------------------------------------------------------
-- TODO: move these to a library

nameSplices :: Monad m => T.Text -> Splices (Splice m)
nameSplices x = do
	"name" ## textSplice x
	"lcname" ## textSplice $ T.toLower x


generalSplices :: Monad m => Splices (Splice m)
generalSplices =
	"archivePath" ## stringSplice Project.archivesDirectory

bindSplices' :: Monad m => Splices (AttrSplice m) -> Splices (Splice m) -> Splice m
bindSplices' attrSplices splices = localHS (bindAttributeSplices attrSplices) $ runChildrenWith splices

ifMaybeSpliceWith :: Monad m => (a -> Splice m) -> Maybe a -> Splice m
ifMaybeSpliceWith splice (Just x) = ifSpliceWith True (splice x) hideContents
ifMaybeSpliceWith _ Nothing = ifSplice False

{----------------------------------------------------------------------------------------------------{
                                                                      | Generic Splices
}----------------------------------------------------------------------------------------------------}

groupedListSplice :: Monad m => T.Text -> (T.Text, [T.Text]) -> Splice m
groupedListSplice tag (x, ys) = runChildrenWith $ do
	"name" ## textSplice x
	tag ## listSplice "name" ys

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

projectComponentMaybeImageSplices :: Monad m => (Project.Project, [(Component.Component, Maybe Image.Image)]) -> Splices (Splice m)
projectComponentMaybeImageSplices = projectComponentSplices mapMaybe (maybeSplice (runChildrenWith . imageSplices))

projectComponentImageListSplices :: Monad m => (Project.Project, [(Component.Component, [Image.Image])]) -> Splices (Splice m)
projectComponentImageListSplices = projectComponentSplices concatMap (listToSplice imageSplices)

-- this is the generalized version of the above functions to eliminate code duplication
projectComponentSplices :: (Monad m, Functor f)
	=> (((Component.Component, f Image.Image) -> f Image.Image) -> [(Component.Component, f Image.Image)] -> [Image.Image])
	-> (f Image.Image -> Splice m)
	-> (Project.Project, [(Component.Component, f Image.Image)])
	-> Splices (Splice m)
projectComponentSplices toImageList imageSplice (p, cx) = do
	projectSplices p
	"component" ## listToSplice cSplices cx
	"image" ## listToSplice imageSplices $ if hasImages then images else []
	where
		images = toImageList snd cx
		hasImages = length images == 1
		cSplices (c, xs) = do
			componentSplices c
			"image" ## if hasImages
				then hideContents
				else imageSplice xs

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
	"archived" ## maybeSplice (\x -> ifSplice $ "/" == T.take 1 x) $ Component.archived c
	"href" ## maybeSplice textSplice $ Component.archived c
	"tag" ## listSplice "name" $ Component.tags c

{----------------------------------------------------------------------------------------------------{
                                                                      | Tag Splices
}----------------------------------------------------------------------------------------------------}

tagSplices :: Monad m => Tag.Tag -> Splices (Splice m)
tagSplices c = do
	"component_type" ## textSplice $ Tag.componentType c
	"tag" ## textSplice $ Tag.tag c
	"category" ## textSplice $ Tag.category c

{----------------------------------------------------------------------------------------------------{
                                                                      | Image Splices
}----------------------------------------------------------------------------------------------------}

imageSplices :: Monad m => Image.Image -> Splices (Splice m)
imageSplices i = do
	"filename" ## textSplice $ Image.filename i
	"width" ## numericSplice $ Image.width i
	"height" ## numericSplice $ Image.height i
	"featured" ## showSplice $ Image.featured i
	"isDefault" ## ifSplice $ Image.featured i

checkedSplice :: Monad m => Bool -> AttrSplice m
checkedSplice True = const $ return [("checked", "checked")]
checkedSplice _    = mempty

{----------------------------------------------------------------------------------------------------{
                                                                      | Archive Serve
}----------------------------------------------------------------------------------------------------}

dateSplice :: (Monad m, FormatTime t) => t -> Splice m
dateSplice = dateFormatSplice defaultTimeLocale "%Y-%m-%d"

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
