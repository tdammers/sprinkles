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

data RuleTarget p =
    TemplateTarget p |
    RedirectTarget p |
    StaticTarget |
    JSONTarget
    deriving (Eq, Show)

data Rule =
    Rule
        { ruleRoutePattern :: Pattern
        , ruleContextData :: HashMap Text BackendSpec
        , ruleTarget :: RuleTarget Replacement
        , ruleRequired :: Set Text
        }
        deriving (Show)

instance FromJSON Rule where
    parseJSON (Object obj) = do
        pattern <- obj .: "pattern"
        contextData <- fromMaybe (mapFromList []) <$> obj .:? "data"
        templateMay <- fmap TemplateTarget <$> (obj .:? "template")
        redirectMay <- fmap RedirectTarget <$> (obj .:? "redirect")
        static <- fromMaybe False <$> (obj .:? "static")
        let target =
                if static
                    then StaticTarget
                    else (fromMaybe JSONTarget $ redirectMay <|> templateMay)
        required <- obj .:? "required" .!= []
        return $ Rule pattern contextData target required

expandRuleTarget :: HashMap Text Text -> RuleTarget Replacement -> RuleTarget Text
expandRuleTarget _ JSONTarget = JSONTarget
expandRuleTarget _ StaticTarget = StaticTarget
expandRuleTarget varMap (TemplateTarget p) = TemplateTarget $ expandReplacement varMap p
expandRuleTarget varMap (RedirectTarget p) = RedirectTarget $ expandReplacement varMap p

applyRule :: Rule -> Text -> Maybe (HashMap Text BackendSpec, Set Text, RuleTarget Text)
applyRule rule query = do
    varMap <- matchPattern (ruleRoutePattern rule) query
    let f :: Replacement -> Text
        f pathPattern = expandReplacement varMap pathPattern
        expandReplacementBackend :: BackendSpec -> BackendSpec
        expandReplacementBackend =
            omap (maybeThrow . expandReplacementText varMap)
    return
        ( fmap expandReplacementBackend (ruleContextData rule)
        , ruleRequired rule
        , expandRuleTarget varMap (ruleTarget rule)
        )

applyRules :: [Rule] -> Text -> Maybe (HashMap Text BackendSpec, Set Text, RuleTarget Text)
applyRules [] _ = Nothing
applyRules (rule:rules) query =
    applyRule rule query <|> applyRules rules query


