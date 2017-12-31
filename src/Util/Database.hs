{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, OverloadedStrings, OverlappingInstances, FlexibleContexts #-}

module Util.Database
	( str
	, sqlFile
	, toEither
	, toEither'
	, manyEithers
	, ConstraintViolation
	) where

import Control.Applicative
--import Control.Exception as E
--import Control.Monad.IO.Class (MonadIO)
import Control.Exception.Lifted (catch, throw)
--import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.Errors
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Snap.Snaplet.PostgresqlSimple

str :: QuasiQuoter
str = QuasiQuoter { quoteExp = stringE }

sqlFile :: QuasiQuoter
sqlFile = quoteFile str

----------------------------------------------------------------------

instance (ToField a) => ToField [a] where
	toField = toField . PGArray

instance (FromField a, Typeable a) => FromField [a] where
	fromField f v = fromPGArray <$> fromField f v

---------------------------------------------------------------------- | Stuff that will go in a library

-- the purpose of this function is to chain a series of Eithers together.
-- When a Left is encountered, we stop and return the Left.  Otherwise, we
-- return the Right.  This way, we get a useful error message rather than a
-- useless error about the transaction being aborted
manyEithers :: Monad m => [m (Either a b)] -> m (Either a b)
manyEithers [] = undefined
manyEithers (x:[]) = x
manyEithers (x:xs) = x >>= either (return . Left) (const (manyEithers xs))

catchViolation' :: (MonadBaseControl IO m) => (SqlError -> ConstraintViolation -> m a) -> m a -> m a
catchViolation' f m = catch m (\e -> maybe (throw e) (f e) $ constraintViolation e)
{-
eitherViolation :: (MonadBaseControl m, Functor m) => m a -> m (Either ConstraintViolation a)
eitherViolation x = catchViolation' (\_ -> return . Left) $ fmap Right x
-}
toEither :: (MonadBaseControl IO m, Functor m) => (ConstraintViolation -> a) -> m b -> m (Either a b)
toEither violationLookup x = catchViolation' (\ _ -> return . Left . violationLookup) $ fmap Right x

---------------------------------------------------------------------- | Stuff that will stay in the application

toEither' :: (MonadBaseControl IO m, Functor m) => m b -> m (Either Text b)
toEither' = toEither violationToError

violationToError :: ConstraintViolation -> Text
violationToError x = fromMaybe (unknownConstraintViolation x) $ x `lookup` constraintErrors

unknownConstraintViolation :: ConstraintViolation -> Text
unknownConstraintViolation (NotNullViolation c) = "Unknown not null constraint violation: " <> decodeUtf8 c
unknownConstraintViolation (UniqueViolation c) = "Unknown unique constraint violation: " <> decodeUtf8 c
unknownConstraintViolation (ForeignKeyViolation t c) = "Unknown foreign key constraint violation: " <> decodeUtf8 t <> "(" <> decodeUtf8 c <> ")"
unknownConstraintViolation (CheckViolation t c) = "Unknown check constraint violation: " <> decodeUtf8 t <> "(" <> decodeUtf8 c <> ")"
unknownConstraintViolation (ExclusionViolation c) = "Unknown exclusion constraint violation: " <> decodeUtf8 c

constraintErrors :: [(ConstraintViolation, Text)]
constraintErrors =
	[ (UniqueViolation "projects_pkey", "That project name already exists")
	, (UniqueViolation "projects_slug_key", "That slug already exists")
	, (UniqueViolation "project_components_pkey", "That type of component already exists for that date")
--	, (ForeignKeyViolation "roster_cover_revisions" "roster_character_revisions_character_fkey", "Character data mismatch (cannot recover from this error normally, try returning to your roster and editing it again)")
--	, (CheckViolation "roster_cover_revisions" "roster_covers_check", "Cover quantity must be from 0-5")
	]
