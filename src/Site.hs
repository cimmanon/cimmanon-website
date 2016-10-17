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
import Snap.Util.FileServe (serveDirectory)
import Snap.Util.GZip (withCompression)
import Heist
import Heist.Interpreted
import Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
import Application
import Splices

import Text.Digestive.Heist
import Snap.Handlers
import Heist.Splices.Camellia

import Control.Monad
import Control.Monad.IO.Class (liftIO) -- just for debugging
import Data.Maybe (fromMaybe)

import qualified Model.Component as Component
import qualified Model.Image as Image
import qualified Model.Project as Project
import qualified Model.Tag as Tag

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes =
	[ ("/", ifTop indexH)
	, ("/projects/", ifTop projectsH)
	, ("/projects/tags/:tag", ifTop $ textParam "tag" >>= maybeH (listByH byTagH <=< Project.listByTag))
	, ("/projects/tags/", ifTop $ listByH byTagH [])
	, ("/projects/year/:year", ifTop $ intParam "year" >>= maybeH (listByH byYearH <=< Project.listByYear))
	, ("/projects/year/", ifTop $ listByH byYearH [])
	, ("/projects/type/:type", ifTop $ textParam "type" >>= maybeH (listByH byTypeH <=< Project.listByType))
	, ("/projects/type/", ifTop $ listByH byTypeH [])
	, ("/projects/:slug/", ifTop $ modelH textParam "slug" Project.get projectH)
	, ("/projects/:slug/:type/:date/", id =<< archiveH' <$> textParam "slug" <*> textParam "type" <*> textParam "date")
	, ("/admin/", adminRoutes)
	, ("/archives/", archiveServe)
	, ("/archives/", serveDirectory "archives")
	, ("", heistServe) -- serve up static templates from your templates directory
	, ("", setCache 604800 >> serveDirectory "static")
	]

adminRoutes :: AppHandler ()
adminRoutes = withSplices aSplices $ route
	[ ("/", ifTop $ redirect "./projects/")
	, ("/projects/", ifTop adminProjectsH)
	, ("/projects/add", ifTop addProjectH)
	, ("/projects/:slug/", modelH textParam "slug" Project.get projectRoutes)
	]
	where
		aSplices = do
			"isProject" ## hideContents
			"isComponent" ## hideContents

projectRoutes :: Project.Project -> AppHandler ()
projectRoutes p =
	withSplices pSplices $ route
		[ ("/", ifTop $ editProjectH p)
		, ("/components/", ifTop $ adminComponentsH p)
		, ("/components/:type/", ifTop $ addComponentH p)
		, ("/components/:type/:date/", id =<< componentRoutes <$> textParam "type" <*> textParam "date")
		]
	where
		componentRoutes (Just c) (Just d) =
			Component.get p c d >>= maybeH (\c' -> withSplices ("isComponent" ## runChildrenWith $ componentSplices c') $ route
				[ ("/", ifTop $ editComponentH p c')
				, ("/images", ifTop $ componentImagesH p c')
				, ("/upload", ifTop $ uploadImagesH p c')
				])
		componentRoutes _ _ = notFound
		pSplices = do
			projectSplices p
			"isProject" ## showContents

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "A portfolio CMS for multi-talented professionals" Nothing $ do
	addRoutes routes

	h <- nestSnaplet "" heist $ heistInit "templates"
	-- ^ default Heist behavior (serves raw templates ahead of configured routes)
	s <- nestSnaplet "sess" sess $
		initCookieSessionManager "site_key.txt" "sess" (Just 3600)
	d <- nestSnaplet "db" db pgsInit

	addTemplatesAt h "archives" "archives" -- for archiveServe

	wrapSite (<|> notFound)
	wrapSite withCompression

	-- only display a pretty error if we are not in development mode
	initPrettyProductionErrors

--	initFlashNotice h sess
	addConfig h $ mempty & scInterpretedSplices .~ defaultSplices
	return $ App h s d
	where
		defaultSplices = do
			userSessionSplices sess
			generalSplices
			"bodyId" ## textSplice "default"

-- TODO: move this to a library
maybeH :: (a -> AppHandler ()) -> Maybe a -> AppHandler ()
maybeH = maybe notFound

{----------------------------------------------------------------------------------------------------{
                                                                      | Portfolio
}----------------------------------------------------------------------------------------------------}

indexH :: AppHandler ()
indexH = do
	projects <- Project.list
	renderWithSplices "index" $ "project" ## listToSplice projectComponentSplices projects

projectsH :: AppHandler ()
projectsH = do
	projects <- Project.adminList
	renderWithSplices "projects/list" $ do "project" ## listToSplice projectSplices projects

listByH :: AppHandler () -> [(Project.Project, [(Component.Component, Maybe Image.Image)])] -> AppHandler ()
listByH handler xs =
	withSplices ("project" ## listToSplice projectComponentSplices xs) handler

byTagH :: AppHandler ()
byTagH = do
	tags <- Tag.list
	renderWithSplices "projects/by_tag" $ "category" ## listToSplice tagCategorySplices tags

byYearH :: AppHandler ()
byYearH = do
	years <- Project.years
	renderWithSplices "projects/by_year" $ "year" ## listToSplice (\x -> "name" ## numericSplice x) years

byTypeH :: AppHandler ()
byTypeH = do
	types <- Component.types
	renderWithSplices "/projects/by_type" $ "type" ## listToSplice nameSplices types

projectH :: Project.Project -> AppHandler ()
projectH p = do
	components <- Component.list p
	renderWithSplices "projects/project" $ projectComponentSplices' (p, components)

{----------------------------------------------------------------------------------------------------{
                                                                      | Administration
}----------------------------------------------------------------------------------------------------}

adminProjectsH :: AppHandler ()
adminProjectsH = do
	projects <- Project.adminList
	renderWithSplices "/projects/admin" $ "project" ## listToSplice projectSplices projects

addProjectH :: AppHandler ()
addProjectH = processForm "form" (Project.projectForm Nothing) Project.add
	(renderWithSplices "/projects/add" . digestiveSplices)
	(\p -> redirect $ "./" <> T.encodeUtf8 (Project.slug p) <> "/components/add")

editProjectH :: Project.Project -> AppHandler ()
editProjectH p = processForm "form" (Project.projectForm (Just p)) (Project.edit p)
	(renderWithSplices "/projects/edit" . digestiveSplices)
	(\p' -> redirect $ "../" <> T.encodeUtf8 (Project.slug p') <> "/components/")

--------------------------------------------------------------------- | Components

adminComponentsH :: Project.Project -> AppHandler ()
adminComponentsH p = do
	components <- Component.adminList p
	renderWithSplices "/components/admin" $ "component" ## listToSplice componentSplices components

addComponentH :: Project.Project -> AppHandler ()
addComponentH p = do
	-- the field name plus a period is 10 characters long, so we want to drop it to get the actual value
	-- TODO: make this nicer
	defaultType <- maybe "" (T.drop 10) <$> textParam "form.type"
	processForm "form" (Component.componentForm (Left defaultType)) (Component.add p)
		(renderWithSplices "/components/add" . digestiveSplices' customDigestiveSplices)
		(\c' -> redirect $ "./" <> T.encodeUtf8 (Component.typ c') <> "/" <> B.pack (show $ Component.date c') <> "/images")

editComponentH :: Project.Project -> Component.Component -> AppHandler ()
editComponentH p c = do
	-- the field name plus a period is 10 characters long, so we want to drop it to get the actual value
	-- TODO: make this nicer
	c' <- maybe c (\x -> c { Component.typ = T.drop 10 x }) <$> textParam "form.type"
	processForm "form" (Component.componentForm (Right c)) (Component.edit p)
		(renderWithSplices "/components/edit" . digestiveSplices' customDigestiveSplices) (const (redirect "../../"))

componentImagesH :: Project.Project -> Component.Component -> AppHandler ()
componentImagesH p c = do
	images <- Image.list p c
	processForm "update" (Image.updateForm images) (Image.update p c)
		(viewH images) (const redirectToSelf)
	where
		viewH images v =
			renderWithSplices "/components/images" $ do
				"image" ## listToSplice iSplices $ zip ([0..] :: [Int]) images
				digestiveSplices' customDigestiveSplices v
		iSplices (i, img) =  do
			imageSplices img
			"indice" ## numericSplice i

uploadImagesH :: Project.Project -> Component.Component -> AppHandler ()
uploadImagesH p c = processForm "upload" Image.uploadForm (Image.add p c)
	(const (redirect "./images")) (const (redirect "./images"))

{----------------------------------------------------------------------------------------------------{
                                                                      | Web Archives
}----------------------------------------------------------------------------------------------------}

archiveServe :: AppHandler ()
archiveServe = do
	url <- withRequest (return . rqPathInfo)
	let
		template = "archives/" <> url
	renderWithSplices template archiveServeSplices <|> renderWithSplices (template <> "index") archiveServeSplices


archiveH' :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> AppHandler ()
archiveH' (Just s) (Just t) (Just d) = maybeH archiveH =<< Project.getWithComponent s t d
archiveH' _ _ _ = notFound

archiveH :: (Project.Project, Component.Component) -> AppHandler ()
archiveH (p, c) = do
	renderWithSplices "components/archive" $ do
		projectSplices p
		componentSplices c
