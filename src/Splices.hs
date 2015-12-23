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

import Heist.Splices.Session
import qualified Model.Project as Project

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