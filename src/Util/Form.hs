{-# LANGUAGE OverloadedStrings #-}
-- TODO: turn this into a library
module Util.Form where

import Data.Monoid
import Data.Text (Text)
import Control.Applicative
import Text.Digestive

----------------------------------------------------------------------

listOfText :: (Monad m, Monoid v) => [Text] -> [Text] -> Form v m [Text]
listOfText a b =
	validate checkboxesValidity $ listOf checkboxForm (Just $ listOfCheckboxes a b)

----------------------------------------------------------------------
-- Helper Functions

listOfCheckboxes :: Eq a => [a] -> [a] -> [(a, Bool)]
listOfCheckboxes allValues selectedValues =
	map (\ x -> (x, x `elem` selectedValues)) allValues

checkboxesToList :: [(a, Bool)] -> [a]
checkboxesToList = map fst . filter snd

checkboxesValidity :: Monoid v => [(a, Bool)] -> Result v [a]
checkboxesValidity = Success . map fst . filter snd

checkboxForm :: (Monad m, Monoid v) => Maybe (Text, Bool) -> Form v m (Text, Bool)
checkboxForm e = ( , )
	<$> "name" .: text (fst <$> e)
	<*> "item" .: bool (snd <$> e)
