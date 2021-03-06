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
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Core
import Snap.Extras.FlashNotice
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

--import Text.Digestive.Heist
import Snap.Handlers
import Heist.Splices.Camellia
import Util.Digestive

import Control.Monad
import Control.Monad.IO.Class (liftIO) -- just for debugging

import qualified Model.Component as Component
import qualified Model.ComponentType as ComponentType
import qualified Model.Image as Image
import qualified Model.Project as Project
import qualified Model.Tag as Tag
import qualified Model.TagCategory as TagCategory

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
	, ("/settings/component-types/", ifTop adminComponentTypesH)
	, ("/settings/tag-categories/", ifTop adminTagCategoriesH)
	, ("/settings/tags/", ifTop adminTagsH)
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
			Component.get p c d >>= maybeH (\c' -> withSplices (do "isComponent" ## showContents; componentSplices c') $ route
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

	initFlashNotice h sess
	addConfig h $ mempty & scInterpretedSplices .~ defaultSplices
	return $ App h s d
	where
		defaultSplices = do
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
	renderWithSplices "index" $ "project" ## listToSplice projectComponentMaybeImageSplices projects

projectsH :: AppHandler ()
projectsH = do
	projects <- Project.adminList
	renderWithSplices "projects/list" $ "project" ## listToSplice projectSplices projects

listByH :: AppHandler () -> [(Project.Project, [(Component.Component, Maybe Image.Image)])] -> AppHandler ()
listByH handler xs =
	withSplices ("project" ## listToSplice projectComponentMaybeImageSplices xs) handler

byTagH :: AppHandler ()
byTagH = do
	tags <- Tag.groupedByCategory
	renderWithSplices "projects/by_tag" $ "category" ## mapSplices (groupedListSplice "tag") tags

byYearH :: AppHandler ()
byYearH = do
	years <- Project.years
	renderWithSplices "projects/by_year" $ "year" ## listToSplice (\x -> "name" ## numericSplice x) years

byTypeH :: AppHandler ()
byTypeH = do
	types <- ComponentType.list
	renderWithSplices "/projects/by_type" $ "type" ## listToSplice nameSplices types

projectH :: Project.Project -> AppHandler ()
projectH p = do
	components <- Component.list p
	renderWithSplices "projects/project" $ projectComponentImageListSplices (p, components)

{----------------------------------------------------------------------------------------------------{
                                                                      | Administration
}----------------------------------------------------------------------------------------------------}

--------------------------------------------------------------------- | Settings

adminComponentTypesH :: AppHandler ()
adminComponentTypesH = do
	components <- ComponentType.list
	processForm "form" (ComponentType.componentTypeForm components) (ComponentType.admin)
		(renderWithSplices "/settings/component_types" . digestiveSplicesCustom)
		(\_ -> do flashSuccess sess "Component types successfully updated"; redirectToSelf)

adminTagCategoriesH :: AppHandler ()
adminTagCategoriesH = do
	categories <- TagCategory.list
	processForm "form" (TagCategory.tagCategoryForm categories) (TagCategory.admin)
		(renderWithSplices "/settings/tag_categories" . digestiveSplicesCustom)
		(\_ -> do flashSuccess sess "Tag categories successfully updated"; redirectToSelf)

adminTagsH :: AppHandler ()
adminTagsH = do
	tags <- Tag.adminList
	case tags of
		[] -> do
			flashError sess "Component types must be initialized before adding tags"
			redirect "/admin/settings/component-types"
		_ -> catchEmptyChoice
			(do
				flashError sess "Tag categories must be initialized before adding tags"
				redirect "/admin/settings/tag-categories"
				) $
			processForm "form" (Tag.tagsForm tags) (Tag.admin)
				(renderWithSplices "/settings/tags" . digestiveSplicesCustom)
				(\_ -> do flashSuccess sess "Tags successfully updated"; redirectToSelf)

--------------------------------------------------------------------- | Projects

adminProjectsH :: AppHandler ()
adminProjectsH = do
	projects <- Project.adminList
	renderWithSplices "/projects/admin" $ "project" ## listToSplice projectSplices projects

addProjectH :: AppHandler ()
addProjectH = processForm "form" (Project.projectForm Nothing) Project.add
	(renderWithSplices "/projects/add" . digestiveSplicesCustom)
	(\p -> do
		flashSuccess sess "Project successfully added"
		redirect $ "./" <> T.encodeUtf8 (Project.slug p) <> "/components/add"
		)

editProjectH :: Project.Project -> AppHandler ()
editProjectH p = processForm "form" (Project.projectForm (Just p)) (Project.edit p)
	(renderWithSplices "/projects/edit" . digestiveSplicesCustom)
	(\p' -> do
		flashSuccess sess "Project successfully updated"
		redirect $ "../" <> T.encodeUtf8 (Project.slug p') <> "/components/"
		)

--------------------------------------------------------------------- | Components

adminComponentsH :: Project.Project -> AppHandler ()
adminComponentsH p = do
	components <- Component.adminList p
	renderWithSplices "/components/admin" $ "component" ## listToSplice splices components
	where
		splices (c, i) = do
			componentSplices c
			"image" ## ifMaybeSpliceWith (runChildrenWith . imageSplices) i

addComponentH :: Project.Project -> AppHandler ()
addComponentH p = do
	defaultType <- maybe "" (T.replace "form.type." "") <$> textParam "form.type"
	catchEmptyChoice
		(do
			flashError sess "Component types must be initialized before adding adding components"
			redirect "/admin/settings/component-types"
			) $
		processForm "form" (Component.componentForm (Left defaultType)) (Component.add p)
			(renderWithSplices "/components/add" . digestiveSplicesCustom)
			(\c' -> do
				flashSuccess sess "Component successfully added"
				redirect $ "./" <> T.encodeUtf8 (Component.typ c') <> "/" <> B.pack (show $ Component.date c') <> "/images"
				)

editComponentH :: Project.Project -> Component.Component -> AppHandler ()
editComponentH p c = do
	c' <- maybe c (\x -> c { Component.typ = T.replace "form.type." "" x }) <$> textParam "form.type"
	processForm "form" (Component.componentForm (Right c')) (Component.edit p)
		(renderWithSplices "/components/edit" . digestiveSplicesCustom)
		(\_ -> do
			flashSuccess sess "Component successfully updated"
			redirect "../../"
			)

componentImagesH :: Project.Project -> Component.Component -> AppHandler ()
componentImagesH p c = do
	images <- Image.list p c
	-- if images is empty, the form will throw an exception because there are no choices
	case images of
		[] -> renderWithSplices "/components/images" $ "dfForm" ## hideContents
		_ -> processForm "update" (Image.updateForm images) (Image.update p c)
			(renderWithSplices "/components/images" . digestiveSplicesCustom)
			(\_ -> do
				flashSuccess sess "Images successfully updated"
				redirectToSelf
				)

uploadImagesH :: Project.Project -> Component.Component -> AppHandler ()
uploadImagesH p c =
	processForm "upload" Image.uploadForm (Image.add p c)
		(const (redirect "./images"))
		(const (do flashSuccess sess "Images successfully uploaded"; redirect "./images"))

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
archiveH (p, c) =
	renderWithSplices "components/archive" $ do
		projectSplices p
		componentSplices c
