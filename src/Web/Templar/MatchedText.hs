{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE AutoDeriveTypeable #-}
{-#LANGUAGE DeriveGeneric #-}
module Web.Templar.MatchedText
where

import ClassyPrelude

import qualified Text.Ginger as Ginger
import Text.Ginger
       (parseGinger, Template, runGingerT, GingerContext, GVal(..), ToGVal(..),
        (~>))
import GHC.Generics

data MatchedText =
    MatchedText Text |
    MatchedTexts [Text]
    deriving (Show, Read, Eq, Generic)

instance ToGVal m MatchedText where
    toGVal (MatchedText t) = toGVal t
    toGVal (MatchedTexts ts) = toGVal ts

instance IsString MatchedText where
    fromString = MatchedText . fromString
