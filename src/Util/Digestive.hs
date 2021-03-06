{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Util.Digestive where

import Control.Arrow (second)
import Control.Applicative
import Control.Exception.Base (SomeException)
import Control.Monad.CatchIO (MonadCatchIO, try, throw)
import Control.Monad.Trans
import Data.List ((\\))
import Data.Maybe (isJust, isNothing, mapMaybe, fromJust)
import Data.Monoid (Monoid, (<>))
import qualified Data.Text as T
import Text.Digestive
import Text.Digestive.Heist hiding (dfSubView)
import Text.Digestive.Heist.Extras
import Heist
import Heist.Interpreted
import qualified Text.XmlHtml as X

--import Text.Digestive.Form (disable)
--import Text.Digestive.Form.List
--import Text.Digestive.View
import Text.Digestive.Heist.Extras.Internal.Attribute (getRefAttributes)

{----------------------------------------------------------------------------------------------------{
                                                                      | Validation
}----------------------------------------------------------------------------------------------------}

notEmptyText :: T.Text -> Bool
notEmptyText x | T.strip x /= "" = True
notEmptyText _ = False

{----------------------------------------------------------------------------------------------------{
                                                                      | Splices
}----------------------------------------------------------------------------------------------------}

digestiveSplicesCustom :: MonadIO m => View T.Text -> Splices (Splice m)
digestiveSplicesCustom v = do
	digestiveSplices v
	"dfPath" ## dfPath v
	"dfSubView" ## dfSubView digestiveSplicesCustom v
	"dfInputList" ## dfInputListCustom digestiveSplicesCustom v
	"dfInputListStatic" ## dfInputListStatic digestiveSplicesCustom v
	"dfScriptValues" ## dfScriptValues v
	"dfPlainText" ## dfPlainText v
	"dfCustomText" ## dfCustomText v
	"dfCustomChoice" ## dfCustomChoice v
	"dfInputListCustom" ## dfInputListCustom' digestiveSplicesCustom v

dfInputListCustom' :: MonadIO m => (View T.Text -> Splices (Splice m)) -> View T.Text -> Splice m
dfInputListCustom' splices view =
	let
		splices' v = do
			splices v
			"dfGroupRadioText" ## dfGroupRadioText view v
	in
		dfInputListCustom splices' view

-- this is a very crude splice that generates a script element containing a var that holds an object
dfScriptValues :: Monad m => View T.Text -> Splice m
dfScriptValues v = do
	(ref, _) <- getRefAttributes Nothing
	let
		xs = fieldInputChoiceGroup ref v
		var = T.concat ["var " <> ref <> " = {", vals, "};"]
		vals = T.intercalate ", " $ map (\(x, ys) -> T.concat [x, ": [", tags ys, "]"]) xs
		tags = T.intercalate ", " . map (\(i, _, _) -> "'" <> i <> "'")
	return [X.Element "script" [("type", "text/javascript")] [X.TextNode var]]

{----------------------------------------------------------------------------------------------------{
                                                                      | Helper Functions
}----------------------------------------------------------------------------------------------------}

textToChoice :: T.Text -> (T.Text, (T.Text, T.Text))
textToChoice = toChoice id id id

toChoice :: (a -> T.Text) -> (a -> b) -> (a -> T.Text) -> a -> (T.Text, (b, T.Text))
toChoice toIdentity toValue toLabel x = (toIdentity x, (toValue x, toLabel x))

{----------------------------------------------------------------------------------------------------{
                                                                      | Forms
}----------------------------------------------------------------------------------------------------}

maybeList :: [a] -> Maybe [a]
maybeList [] = Nothing
maybeList xs = Just xs

nameForm :: (Monad m, Monoid v) => Maybe T.Text -> Form v m T.Text
nameForm x = "name" .: text x

-- simple list form where the primary key for editing the record is part of the element's original contents
itemsForm :: (Monad m, Monoid v, Eq b) => (a -> b) -> (Maybe a -> Form v m a) -> Maybe [a] -> Form v m ([(a, Maybe b)], [b])
itemsForm extractKey form xs = validate validateItems $ listOf itemForm xs
	where
		itemForm x = ( , )
			<$> form x
			<*> "primary_key" .: pure (extractKey <$> x)
		validateItems ys = Success (ys, maybe [] (map extractKey) xs \\ mapMaybe snd ys)

extractInserts :: [(a, Maybe b)] -> [a]
extractInserts = map fst . filter (isNothing . snd)

extractUpdates :: [(a, Maybe b)] -> [(a, b)]
extractUpdates = map (second fromJust) . filter (isJust . snd)

extractRenames :: Eq a => [(a, Maybe a)] -> [(a, a)]
extractRenames = map (second fromJust) . filter isEdit
	where
		isEdit (a, Just b) = a /= b
		isEdit _ = False

{----------------------------------------------------------------------------------------------------{
                                                                      | Error Handling
}----------------------------------------------------------------------------------------------------}

catchEmptyChoice :: (MonadCatchIO m, Functor m) => m () -> m () -> m ()
catchEmptyChoice handler action = do
	res' :: Either SomeException () <- try action
	case res' of
		Left e | show e == "choice expects a list with at least one item in it" -> handler
		Left e -> throw e
		Right r -> return r
