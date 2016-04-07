{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Templar.PandocGVal
where

import ClassyPrelude hiding (asText, asList)
import Text.Ginger as Ginger (GVal (..), ToGVal (..), dict, (~>))
import Text.Ginger.Html (unsafeRawHtml)
import Text.Pandoc
import Data.Default (def)
import Data.Aeson (ToJSON (..))

writerOptions :: WriterOptions
writerOptions =
    def { writerStandalone = False
        , writerHtml5 = True
        }

instance ToGVal m Pandoc where
    toGVal pandoc@(Pandoc meta blocks) =
        def { asList = Just $ map toGVal blocks
            , asDictItems = Just $ [("meta", toGVal meta), ("body", toGVal blocks)]
            , asLookup = Just $ \case
                            "meta" -> Just (toGVal meta)
                            "body" -> Just (toGVal blocks)
                            _ -> Nothing
            , asHtml = unsafeRawHtml . pack . writeHtmlString writerOptions $ pandoc
            , asText = pack . writePlain writerOptions $ pandoc
            , asBoolean = True
            , asNumber = Nothing
            , asFunction = Nothing
            , Ginger.length = Just (ClassyPrelude.length blocks)
            , isNull = False
            }

instance ToGVal m Meta where
    toGVal meta =
        let entries :: [(String, MetaValue)]
            entries = mapToList . unMeta $ meta
        in dict [ pack key ~> value | (key, value) <- entries ]

instance ToGVal m MetaValue where
    toGVal (MetaMap m) = dict [ pack key ~> value | (key, value) <- mapToList m ]
    toGVal (MetaList values) = toGVal values
    toGVal (MetaBool b) = toGVal b
    toGVal (MetaString str) = toGVal str
    toGVal (MetaInlines inlines) = toGVal inlines
    toGVal (MetaBlocks blocks) = toGVal blocks

instance ToGVal m Block where
    toGVal block = toGVal (Pandoc nullMeta [block])

instance ToGVal m Inline where
    toGVal inline = toGVal (Pandoc nullMeta [Plain [inline]])
