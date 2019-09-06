{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE LambdaCase #-}
module Web.Sprinkles.Rule
where

import Web.Sprinkles.Prelude
import Web.Sprinkles.Pattern
import Web.Sprinkles.Replacement
import Web.Sprinkles.Backends
import Web.Sprinkles.MatchedText
import Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Control.MaybeEitherMonad
import Network.HTTP.Types.URI (QueryText)
import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Data.AList as AList
import Data.AList (AList)
import Text.Ginger (Run, ToGVal (..), GVal, SourcePos)
import Control.Monad.Writer (Writer)
import qualified Data.Set as Set
import Data.Expandable

data TemplateTargetMode =
      AutoTemplateMode |
      GingerHtmlTemplateMode |
      GingerTextTemplateMode
      deriving (Eq, Show, Enum, Ord, Bounded)

instance FromJSON TemplateTargetMode where
  parseJSON (String str) =
    case str of
      "auto" -> pure AutoTemplateMode
      "ginger-html" -> pure GingerHtmlTemplateMode
      "html" -> pure GingerHtmlTemplateMode
      "ginger-text" -> pure GingerTextTemplateMode
      "text" -> pure GingerTextTemplateMode
      x -> fail $ "Invalid template type " ++ show x
  parseJSON Null =
    pure AutoTemplateMode
  parseJSON x =
    fail $ "Invalid template type " ++ show x

data RuleTarget p =
    TemplateTarget TemplateTargetMode p |
    RedirectTarget p |
    StaticTarget (Maybe p) |
    JSONTarget
    deriving (Eq, Show)

type Seconds = Integer

data ClientCacheSetting =
    NoCache |
    CacheForever |
    MaxAge Seconds
    deriving (Eq, Show)

instance FromJSON ClientCacheSetting where
    parseJSON = \case
        String "no-cache" -> return NoCache
        String "forever" -> return CacheForever
        String x -> fail $ "Invalid cache expiry: " ++ show x
        Number n -> return $ MaxAge (floor n)
        _ -> fail "Invalid client cache setting"

-- | Describes if and how to initialize a session for a request.
data SessionDirective = AcceptSession -- ^ Accept if given, but do not require
                      | IgnoreSession -- ^ Ignore all sessions
                      | CreateNewSession -- ^ Always create a new session
                      | RequireSession -- ^ Require a session, fail if none exists
    deriving (Eq, Show)

instance FromJSON SessionDirective where
    parseJSON = \case
        String "ignore" -> return IgnoreSession
        String "new" -> return CreateNewSession
        String "accept" -> return AcceptSession
        String "require" -> return RequireSession
        _ -> fail "Invalid session directive"

data Rule =
    Rule
        { ruleRoutePattern :: Pattern
        , ruleContextData :: AList Text BackendSpec
        , ruleTarget :: RuleTarget Replacement
        , ruleRequired :: Set Text
        , ruleAcceptedMethods :: Set HTTP.Method
        , ruleCaching :: ClientCacheSetting
        , ruleContentTypeOverride :: Maybe ByteString
        , ruleSessionDirective :: SessionDirective
        }
        deriving (Show)

makeRulePathsAbsolute :: FilePath -> Rule -> Rule
makeRulePathsAbsolute dir rule =
  rule
    { ruleContextData =
        fmap (makeBackendSpecPathsAbsolute dir) (ruleContextData rule)
    }

orElse :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
orElse leftAction rightAction = do
    leftAction >>= maybe rightAction (return . Just)

parseTemplateTarget :: Value -> JSON.Parser (Maybe (RuleTarget Replacement))
parseTemplateTarget (Object obj) = do
  mode <- obj .:? "type" .!= AutoTemplateMode
  name <- obj .:? "name"
  pure $ TemplateTarget mode <$> name
parseTemplateTarget s@(String _) =
  Just . TemplateTarget AutoTemplateMode <$> parseJSON s
parseTemplateTarget Null =
  pure Nothing
parseTemplateTarget x =
  fail $ "Unexpected data type for template target: " ++ show x

instance FromJSON Rule where
    parseJSON (Object obj) = do
        pattern <- obj .: "pattern"
        (methodsMay :: Maybe [Text]) <- obj .:? "methods"
        (methodMay :: Maybe [Text]) <- fmap (:[]) <$> (obj .:? "method")
        let methods = Set.fromList . map encodeUtf8 . fromMaybe [ "GET", "POST" ] $
                methodsMay <|> methodMay
        contextData <- fromMaybe AList.empty <$> obj .:? "data"
        templateMay <- parseTemplateTarget =<< obj .:? "template" .!= Null
        redirectMay <- fmap RedirectTarget <$> (obj .:? "redirect")
        static <- fromMaybe False <$> (obj .:? "static")
        staticChildPath <- obj .:? "child"
        let target =
                if static
                    then StaticTarget staticChildPath
                    else (fromMaybe JSONTarget $ redirectMay <|> templateMay)
        required <- obj .:? "required" .!= []
        caching <- obj .:? "cache" .!= CacheForever
        contentTypeOverride <-
            (fmap encodeUtf8) <$>
                (obj .:? "content-type")
        sessionDirective <- obj .:? "session" .!= AcceptSession
        return $ Rule
            pattern
            contextData
            target
            required
            methods
            caching
            contentTypeOverride
            sessionDirective
    parseJSON x = fail $ "Expected rule, but found " <> show x

expandRuleTarget :: HashMap Text (GVal (Run SourcePos IO Text)) -> RuleTarget Replacement -> IO (RuleTarget Text)
expandRuleTarget _ JSONTarget =
     return JSONTarget
expandRuleTarget varMap (StaticTarget pMay) =
     StaticTarget <$> mapM (expandReplacement varMap) pMay
expandRuleTarget varMap (TemplateTarget m p) =
     TemplateTarget m <$> expandReplacement varMap p
expandRuleTarget varMap (RedirectTarget p) =
     RedirectTarget <$> expandReplacement varMap p

expandReplacementBackend :: HashMap Text (GVal (Run SourcePos IO Text))
                         -> BackendSpec
                         -> IO BackendSpec
expandReplacementBackend varMap spec = do
    bsType' <- expandM (expandReplacementText varMap) (bsType spec)
    return $ spec { bsType = bsType' }

data NonMatchReason =
    PathNotMatched | MethodNotMatched
    deriving (Eq, Ord, Enum, Show, Read)

-- | Alternative-like monoid append operator for Eithers over orderable Lefts.
-- The behavior is almost exactly like Alternative proper, except that when
-- both sides fail, the larger failure value prevails.
--
-- In other words:
--
-- Left 3 <|+> Left 2 == Left 3
-- Left 2 <|+> Left 3 == Left 3
-- Left 2 <|+> Right "Hello" == Right "Hello"
-- Right "Hello" <|+> Left 2 == Right "Hello"
-- Right "Hello" <|+> Right "Hello" == Right "Hello"
(<|+>) :: Ord e => Either e a -> Either e a -> Either e a
Left e1 <|+> Left e2 = Left (max e1 e2)
Left _ <|+> Right a = Right a
Right a <|+> _ = Right a

matchMethod :: Set HTTP.Method -> HTTP.Method -> Maybe HTTP.Method
matchMethod acceptedMethods method =
    if method `elem` acceptedMethods
        then Just method
        else Nothing

matchRule :: Rule -> HTTP.Method -> [Text] -> QueryText -> Either NonMatchReason (HashMap Text MatchedText)
matchRule rule method path query = do
    captures <- maybe (Left PathNotMatched) Right $
        matchPattern (ruleRoutePattern rule) path query
    maybe (Left MethodNotMatched) Right $
        matchMethod (ruleAcceptedMethods rule) method
    return captures

applyRule :: Rule
          -> HTTP.Method
          -> [Text]
          -> QueryText
          -> Either
                NonMatchReason
                ( Rule
                , HashMap Text MatchedText
                )
applyRule rule method path query = do
    captures <- matchRule rule method path query
    return (rule, captures)

applyRules :: [Rule]
           -> HTTP.Method
           -> [Text]
           -> QueryText
           -> Either
                 NonMatchReason
                 ( Rule
                 , HashMap Text MatchedText
                )
applyRules (rule:rules) method path query =
    applyRule rule method path query <|+> applyRules rules method path query
applyRules _ _ _ _ = Left PathNotMatched
