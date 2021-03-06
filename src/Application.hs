{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.State (get)
import Control.Monad.Reader (local)
import Snap.Snaplet
import Snap.Snaplet.Heist
--import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.PostgresqlSimple

------------------------------------------------------------------------------
data App = App
	{ _heist :: Snaplet (Heist App)
	, _sess :: Snaplet SessionManager
--	, _auth :: Snaplet (AuthManager App)
	, _db :: Snaplet Postgres
	}

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
	getPostgresState = with db get
	setLocalPostgresState s = local (set (db . snapletValue) s)

------------------------------------------------------------------------------
type AppHandler = Handler App App


