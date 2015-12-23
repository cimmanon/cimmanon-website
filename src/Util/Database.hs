{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeOperators, OverloadedStrings #-}

module Util.Database
	( str
	, sqlFile
	, toEither
	, toEither'
	, ConstraintViolation
	) where

--import Control.Exception as E
--import Control.Monad.IO.Class (MonadIO)
import Control.Monad.CatchIO
--import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple.Errors
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Snap.Snaplet.PostgresqlSimple

str :: QuasiQuoter
str = QuasiQuoter { quoteExp = stringE }

sqlFile :: QuasiQuoter
sqlFile = quoteFile str

---------------------------------------------------------------------- | Stuff that will go in a library

catchViolation' :: (MonadCatchIO m) => (SqlError -> ConstraintViolation -> m a) -> m a -> m a
catchViolation' f m = catch m (\e -> maybe (throw e) (f e) $ constraintViolation e)
{-
eitherViolation :: (MonadCatchIO m, Functor m) => m a -> m (Either ConstraintViolation a)
eitherViolation x = catchViolation' (\_ -> return . Left) $ fmap Right x
-}
toEither :: (MonadCatchIO m, Functor m) => (ConstraintViolation -> a) -> m b -> m (Either a b)
toEither violationLookup x = catchViolation' (\ _ -> return . Left . violationLookup) $ fmap Right x

---------------------------------------------------------------------- | Stuff that will stay in the application

toEither' :: (MonadCatchIO m, Functor m) => m b -> m (Either Text b)
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
	[ (UniqueViolation "users_name_key", "That name already exists")
	, (UniqueViolation "users_email_key", "That email address already exists")
	, (UniqueViolation "rosters_pkey", "That roster already exists")
	, (UniqueViolation "rosters_slug_key", "A roster with that URL already exists")
	, (ForeignKeyViolation "roster_cover_revisions" "roster_cover_revisions_name_fkey", "That character already exists")
	, (ForeignKeyViolation "roster_cover_revisions" "roster_character_revisions_character_fkey", "Character data mismatch (cannot recover from this error normally, try returning to your roster and editing it again)")
	, (CheckViolation "roster_cover_revisions" "roster_covers_check", "Cover quantity must be from 0-5")
	, (CheckViolation "roster_revisions" "roster_cover_revisions_max_covers_check", "Total covers must be between 1 and 13")
	, (CheckViolation "roster_revisions" "roster_level_cap_check", "Character's level cannot exceed the level cap")
	, (CheckViolation "roster_revisions" "roster_revisions_history_duplicate_check", "That revision already exists")
	]
