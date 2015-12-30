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
import qualified Model.Project as Project
import qualified Model.Image as Image

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

componentSplices :: Monad m => Project.Component -> Splices (Splice m)
componentSplices c = do
	"component" ## textSplice $ Project.component c
	"description" ## textSplice $ Project.desc c
	"date" ## showSplice $ Project.date c
	"tag" ## listSplice "name" $ Project.tags c

{----------------------------------------------------------------------------------------------------{
                                                                      | Image Splices
}----------------------------------------------------------------------------------------------------}

imageSplices :: Monad m => Image.Image -> Splices (Splice m)
imageSplices i = do
	"filename" ## textSplice $ Image.filename i
	"width" ## numericSplice $ Image.width i
	"height" ## numericSplice $ Image.height i
