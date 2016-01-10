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
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

import Text.Digestive.Heist
import Snap.Handlers
import Snap.Util.FileUploads
import Heist.Splices.Common

import Control.Monad
import Control.Monad.IO.Class (liftIO) -- just for debugging
import Data.Maybe (isJust, fromJust, fromMaybe, mapMaybe)

import qualified Model.Component as Component
import qualified Model.Image as Image
import qualified Model.Project as Project
import qualified Model.Tag as Tag

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes =
	[ ("/", ifTop indexH)
	, ("/projects/tags/:tag", ifTop $ textParam "tag" >>= maybeH (listByH tagsH <=< Project.listByTag))
	, ("/projects/tags/", ifTop $ listByH tagsH [])
	, ("/projects/year/:year", ifTop $ intParam "year" >>= maybeH (listByH yearH <=< Project.listByYear))
	, ("/projects/year/", ifTop $ listByH yearH [])
	, ("/projects/component/:component", ifTop $ textParam "component" >>= maybeH (listByH componentH <=< Project.listByComponent))
	, ("/projects/component/", ifTop $ listByH componentH [])
	, ("/projects/:slug/", ifTop $ modelH textParam "slug" Project.get projectH)
	, ("/admin/", route adminRoutes)
	, ("/archives/", archiveServe)
	, ("/archives/", serveDirectory "archives")
	, ("", heistServe) -- serve up static templates from your templates directory
	, ("", serveDirectory "static")
	]

adminRoutes :: [(ByteString, AppHandler ())]
adminRoutes =
	[ ("/", ifTop adminListH)
	, ("/project/:slug/", modelH textParam "slug" Project.get (route . projectRoutes))
	]

projectRoutes :: Project.Project -> [(ByteString, AppHandler ())]
projectRoutes p =
	[ ("/", ifTop $ modelH textParam "slug" Project.get editProjectH)
	, ("/components/", ifTop $ modelH textParam "slug" Project.get (`projectComponentsH` Nothing))
	, ("/components/:component/", ifTop $ textParam "component" >>= projectComponentsH p)
	, ("/components/:component/:date/", id =<< componentRoutes <$> textParam "component" <*> textParam "date")
	]
	where
		componentRoutes (Just c) (Just d) =
			Component.get p c d >>= maybeH (\c' -> route
				[ ("/", ifTop $ editComponentH p c')
				, ("/upload", ifTop $ uploadH p c')
				])
		componentRoutes _ _ = pass

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
	addTemplatesAt h "archives" "archives" -- for archiveServe

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

-- TODO: move this to a library
maybeH :: (a -> AppHandler ()) -> Maybe a -> AppHandler ()
maybeH = maybe pass

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
	renderWithSplices "projects/by_tag" $ "category" ## listToSplice tagCategorySplices tags

yearH :: AppHandler ()
yearH = do
	years <- Project.years
	renderWithSplices "projects/by_year" $ "year" ## listToSplice (\x -> "name" ## numericSplice x) years

componentH :: AppHandler ()
componentH = render "/projects/by_component"

projectH :: Project.Project -> AppHandler ()
projectH p = do
	components <- Component.list p
	let
		splices = do
			projectSplices p
			"component" ## listToSplice cSplices components
		cSplices (c, xs) = do
			componentSplices c
			"image" ## listToSplice imageSplices xs
	renderWithSplices "projects/project" splices

{----------------------------------------------------------------------------------------------------{
                                                                      | Administration
}----------------------------------------------------------------------------------------------------}

-- view list of projects & add new ones
adminListH :: AppHandler ()
adminListH = processForm "form" (Project.projectForm Nothing) Project.add
	viewH (\p -> redirect $ "project/" <> T.encodeUtf8 (Project.slug p) <> "/components/")
	where
		viewH v = do
			projects <- Project.adminList
			renderWithSplices "/projects/admin" $ do
				"project" ## listToSplice projectSplices projects
				digestiveSplices v

editProjectH :: Project.Project -> AppHandler ()
editProjectH p = processForm "form" (Project.projectForm (Just p)) (Project.edit p)
	(renderWithSplices "/projects/edit" . digestiveSplices) (const redirectToSelf)

----------------------------------------------------------------------

projectComponentsH :: Project.Project -> Maybe T.Text -> AppHandler ()
projectComponentsH p c = processForm "form" (Component.componentForm (Left defaultComp)) (Component.add p)
	viewH (\c' -> redirect $ "./" <> T.encodeUtf8 (Component.path c') <> "/")
	where
		-- TODO: pull this from the database
		defaultComp = fromMaybe "Design" c
		viewH v =  do
			components <- Component.adminList p
			types <- Component.types
			renderWithSplices "/components/admin" $ do
				projectSplices p
				"component" ## listToSplice componentSplices components
				"type" ## listSplice "name" types
				digestiveSplices v

editComponentH :: Project.Project -> Component.Component -> AppHandler ()
editComponentH p c = processForm "form" (Component.componentForm (Right c)) (Component.edit p)
	viewH (const redirectToSelf)
	where
		viewH v = do
			images <- Image.list p c
			renderWithSplices "/components/edit" $ do
				projectSplices p
				"image" ## listToSplice imageSplices images
				digestiveSplices v

uploadH :: Project.Project -> Component.Component -> AppHandler ()
uploadH p c = processForm "form" Image.imageForm (Image.add p c)
	(renderWithSplices "/components/edit" . digestiveSplices) (const (redirect "./"))

{----------------------------------------------------------------------------------------------------{
                                                                      | Web Archives
}----------------------------------------------------------------------------------------------------}

archiveServe :: AppHandler ()
archiveServe = do
	url <- withRequest (return . rqPathInfo)
	let
		template = "archives/" <> url
	renderWithSplices template archiveServeSplices <|> renderWithSplices (template <> "index") archiveServeSplices
