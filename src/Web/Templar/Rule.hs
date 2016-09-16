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

expandRuleTarget :: HashMap Text Text -> RuleTarget Replacement -> RuleTarget Text
expandRuleTarget _ JSONTarget = JSONTarget
expandRuleTarget _ StaticTarget = StaticTarget
expandRuleTarget varMap (TemplateTarget p) = TemplateTarget $ expandReplacement varMap p
expandRuleTarget varMap (RedirectTarget p) = RedirectTarget $ expandReplacement varMap p

applyRule :: Rule
          -> [Text]
          -> QueryText
          -> Maybe ( AList Text BackendSpec
                   , Set Text
                   , RuleTarget Text
                   )
applyRule rule path query = do
    varMap <- matchPattern (ruleRoutePattern rule) path query
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

applyRules :: [Rule]    
           -> [Text]
           -> QueryText
           -> Maybe ( AList Text BackendSpec
                    , Set Text
                    , RuleTarget Text
                    )
applyRules [] _ _ = Nothing
applyRules (rule:rules) path query =
    applyRule rule path query <|> applyRules rules path query
