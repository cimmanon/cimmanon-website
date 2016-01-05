{-# LANGUAGE OverloadedStrings #-}

module Splices where

------------------------------------------------------------------------------

import qualified Data.Text as T
import Heist.Interpreted
import Heist.SpliceAPI
--import qualified Text.XmlHtml as X

-- for Session stuff
import Snap.Snaplet (SnapletLens, withTop)
import Snap.Snaplet.Session (SessionManager)
import Snap.Snaplet.Heist (SnapletISplice)

--import Data.Monoid ((<>))
import Heist.Splices.Common

import Heist.Splices.Session
import qualified Model.Component as Component
import qualified Model.Image as Image
import qualified Model.Project as Project
import qualified Model.Tag as Tag

----------------------------------------------------------------------
-- TODO: move these to a library

maybeSplice :: Monad m => (a -> Splice m) -> Maybe a -> Splice m
maybeSplice = maybe hideContents

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
                                                                      | Project Splices
}----------------------------------------------------------------------------------------------------}

projectSplices :: Monad m => Project.Project -> Splices (Splice m)
projectSplices p = do
	"name" ## textSplice $ Project.name p
	"description" ## textSplice $ Project.description p
	"slug" ## textSplice $ Project.slug p
	"url" ## maybeSplice (\x -> runChildrenWith $ "href" ## textSplice x) $ Project.url p

{----------------------------------------------------------------------------------------------------{
                                                                      | Component Splices
}----------------------------------------------------------------------------------------------------}

componentSplices :: Monad m => Component.Component -> Splices (Splice m)
componentSplices c = do
	"component" ## textSplice $ Component.component c
	"description" ## textSplice $ Component.description c
	"date" ## showSplice $ Component.date c
	"archived" ## toggleSplice $ Component.archived c
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
