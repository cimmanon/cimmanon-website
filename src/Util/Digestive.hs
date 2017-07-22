{-# LANGUAGE OverloadedStrings #-}

module Util.Digestive where

import Control.Arrow (second, first)
import Control.Monad.Trans
import qualified Data.Text as T
import Text.Digestive.Heist
import Text.Digestive.Heist.Extras
import Heist
import Heist.Interpreted
import qualified Text.XmlHtml as X
import Data.Monoid ((<>))

--import Text.Digestive.Form (disable)
--import Text.Digestive.Form.List
import Text.Digestive.View

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
digestiveSplicesCustom = digestiveSplices' splices
	where
		splices v = do
			"dfPath" ## dfPath v
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
			"dfGroupRadioChoice" ## dfGroupRadioChoice view v
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

toOptionalChoices :: [(T.Text, (a, T.Text))] -> [(T.Text, (Maybe a, T.Text))]
toOptionalChoices xs = ("all", (Nothing, "- All -")) : map (second (first Just)) xs

boolChoices :: [(T.Text, (Maybe Bool, T.Text))]
boolChoices = toOptionalChoices [("yes", (True, "Yes")), ("no", (False, "No"))]

scoreChoices :: [(T.Text, (Int, T.Text))]
scoreChoices = map (toChoice (T.pack . show) id (T.pack . show)) [1..10]
