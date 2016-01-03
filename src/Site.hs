{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
	( app
	) where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Data.Text as T
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
import Heist
import Heist.Interpreted
import Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
import Application
import Splices

import Snap.Handlers
import Heist.Splices.Common

import Control.Monad
import Data.Maybe (isJust, fromJust, mapMaybe)

import qualified Model.Component as Component
import qualified Model.Image as Image
import qualified Model.Project as Project
import qualified Model.Tag as Tag

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes =
	[ ("/", ifTop indexH)
	, ("/projects/tags/:tag", ifTop $ textParam "tag" >>= maybe pass (listByH tagsH <=< Project.listByTag))
	, ("/projects/tags/", ifTop $ listByH tagsH [])
	, ("/projects/year/:year", ifTop $ intParam "year" >>= maybe pass (listByH yearH <=< Project.listByYear))
	, ("/projects/year/", ifTop $ listByH yearH [])
	, ("/projects/component/:component", ifTop $ textParam "component" >>= maybe pass (listByH componentH <=< Project.listByComponent))
	, ("/projects/component/", ifTop $ listByH componentH [])
	, ("/projects/:slug/", ifTop $ modelH textParam "slug" Project.get projectH)
	, ("", heistServe) -- serve up static templates from your templates directory
	, ("", serveDirectory "static")
	]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
--	h <- nestSnaplet "" heist $ heistInit "templates"
	-- ^ default Heist behavior (serves raw templates ahead of configured routes)
	h <- nestSnaplet "" heist $ heistInit' "templates" defaultHeistConfig
	-- ^ removes setting up routes for your templates, allowing your routes to take precidence
	s <- nestSnaplet "sess" sess $
		initCookieSessionManager "site_key.txt" "sess" (Just 3600)
	d <- nestSnaplet "db" db pgsInit

	addRoutes routes

	wrapSite (<|> notFound)

	-- only display a pretty error if we are not in development mode
	initPrettyProductionErrors

--	initFlashNotice h sess
	addConfig h $ mempty & scInterpretedSplices .~ userSessionSplices sess
	return $ App h s d
	where
		-- I don't understand any of this, but it works
		-- borrowed from Snap.Snaplet.Heist.Internal.gHeistInit
		-- why doesn't Snap make defaultConfig accessible?  it would make life easier
		sc = (.~) scLoadTimeSplices defaultLoadTimeSplices mempty
		defaultHeistConfig = emptyHeistConfig
			& hcSpliceConfig .~ sc
			& hcNamespace .~ ""
			& hcErrorNotBound .~ True

{----------------------------------------------------------------------------------------------------{
                                                                      | Handlers
}----------------------------------------------------------------------------------------------------}

indexH :: AppHandler ()
indexH = do
	projects <- Project.list
	let
		splices (p, cx) = do
			projectSplices p
			"component" ## listToSplice (componentSplices . fst) cx
			"image" ## listToSplice imageSplices $ mapMaybe snd cx
	renderWithSplices "index" $ "project" ## listToSplice splices projects

listByH :: AppHandler () -> [(Project.Project, [(Component.Component, Maybe Image.Image)])] -> AppHandler ()
listByH handler xs =
	let
		splices (p, cx) = do
			projectSplices p
			"component" ## listToSplice cSplices cx
		cSplices (c, i) = do
			componentSplices c
			"image" ## maybeSplice (runChildrenWith . imageSplices) i
	in withSplices ("project" ## listToSplice splices xs) handler

tagsH :: AppHandler ()
tagsH = do
	tags <- Tag.list
	renderWithSplices "portfolio/tags" $ "category" ## listToSplice tagCategorySplices tags

yearH :: AppHandler ()
yearH = do
	years <- Project.years
	renderWithSplices "portfolio/year" $ "year" ## listToSplice (\x -> "name" ## numericSplice x) years

componentH :: AppHandler ()
componentH = render "/portfolio/by_component"

projectH :: Project.Project -> AppHandler ()
projectH p = do
	components <- Component.list $ Project.name p
	let
		splices = do
			projectSplices p
			"component" ## listToSplice cSplices components
		cSplices (c, xs) = do
			componentSplices c
			"image" ## listToSplice imageSplices xs
	renderWithSplices "portfolio/project" splices
