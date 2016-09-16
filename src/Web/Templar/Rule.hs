{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
module Web.Templar.Rule
where

import ClassyPrelude
import Web.Templar.Pattern
import Web.Templar.Replacement
import Web.Templar.Backends
import Data.Aeson as JSON
import Control.MaybeEitherMonad
import Network.HTTP.Types.URI (QueryText)
import qualified Data.AList as AList
import Data.AList (AList)
import Text.Ginger (Run, ToGVal (..), GVal)
import Control.Monad.Writer (Writer)

data RuleTarget p =
    TemplateTarget p |
    RedirectTarget p |
    StaticTarget |
    JSONTarget
    deriving (Eq, Show)

data Rule =
    Rule
        { ruleRoutePattern :: Pattern
        , ruleContextData :: AList Text BackendSpec
        , ruleTarget :: RuleTarget Replacement
        , ruleRequired :: Set Text
        }
        deriving (Show)

instance FromJSON Rule where
    parseJSON (Object obj) = do
        pattern <- obj .: "pattern"
        contextData <- fromMaybe AList.empty <$> obj .:? "data"
        templateMay <- fmap TemplateTarget <$> (obj .:? "template")
        redirectMay <- fmap RedirectTarget <$> (obj .:? "redirect")
        static <- fromMaybe False <$> (obj .:? "static")
        let target =
                if static
                    then StaticTarget
                    else (fromMaybe JSONTarget $ redirectMay <|> templateMay)
        required <- obj .:? "required" .!= []
        return $ Rule pattern contextData target required
    parseJSON x = fail $ "Expected rule, but found " <> show x

expandRuleTarget :: HashMap Text (GVal (Run (Writer Text) Text)) -> RuleTarget Replacement -> RuleTarget Text
expandRuleTarget _ JSONTarget = JSONTarget
expandRuleTarget _ StaticTarget = StaticTarget
expandRuleTarget varMap (TemplateTarget p) = TemplateTarget $ expandReplacement varMap p
expandRuleTarget varMap (RedirectTarget p) = RedirectTarget $ expandReplacement varMap p

expandReplacementBackend :: HashMap Text (GVal (Run (Writer Text) Text))
                         -> BackendSpec
                         -> BackendSpec
expandReplacementBackend varMap = omap (maybeThrow . expandReplacementText varMap)

applyRule :: Rule
          -> HashMap Text (GVal (Run (Writer Text) Text))
          -> [Text]
          -> QueryText
          -> Maybe ( Rule
                   , HashMap Text Text
                   )
applyRule rule context path query = do
    captures <- matchPattern (ruleRoutePattern rule) path query
    return (rule, captures)

applyRules :: [Rule]    
           -> HashMap Text (GVal (Run (Writer Text) Text))
           -> [Text]
           -> QueryText
           -> Maybe ( Rule
                    , HashMap Text Text
                    )
applyRules [] _ _ _ = Nothing
applyRules (rule:rules) context path query =
    applyRule rule context path query <|> applyRules rules context path query
