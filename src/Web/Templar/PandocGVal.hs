{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Web.Templar.PandocGVal
where

import ClassyPrelude hiding (asText, asList)
import Text.Ginger as Ginger (GVal (..), ToGVal (..), dict, (~>))
import Text.Ginger.Html (unsafeRawHtml)
import Text.Pandoc
import Text.Pandoc.Walk (query)
import Data.Default (def)
import Data.Aeson (ToJSON (..))
import Data.Scientific (fromFloatDigits)

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
    toGVal block =
        let pandoc = Pandoc nullMeta [block]
            listItems =
                (map toGVal $ (query (:[]) block :: [Inline])) ++
                (map toGVal $ (query (:[]) block :: [Block]))
        in def { asList = Just listItems
               , asDictItems = Just $ mapToList (blockProperties block)
               , asLookup = Just $ \key -> lookup key (blockProperties block)
               , asHtml = unsafeRawHtml . pack . writeHtmlString writerOptions $ pandoc
               , asText = pack . writePlain writerOptions $ pandoc
               , asBoolean = True
               , asNumber = Nothing
               , asFunction = Nothing
               , Ginger.length = Just (ClassyPrelude.length listItems)
               , isNull = False
               }

blockProperties :: forall m. Block -> HashMap Text (GVal m)
blockProperties (Plain _) = mapFromList ["type" ~> ("plain" :: Text)]
blockProperties (Para _) = mapFromList ["type" ~> ("p" :: Text)]
blockProperties (CodeBlock (id, classes, attrs) _) =
    mapFromList
        [ "type" ~> ("code" :: Text)
        , "id" ~> id
        , "classes" ~> classes
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
blockProperties (RawBlock (Format fmt) _) =
    mapFromList
        [ "type" ~> ("raw" :: Text)
        , "format" ~> fmt
        ]
blockProperties (BlockQuote _) = mapFromList ["type" ~> ("blockquote" :: Text)]
blockProperties (OrderedList _ items) =
    mapFromList
        [ "type" ~> ("ol" :: Text)
        , "items" ~> items
        ]
blockProperties (BulletList items) =
    mapFromList
        [ "type" ~> ("ul" :: Text)
        , "items" ~> items
        ]
blockProperties (DefinitionList pairs) =
    mapFromList
        [ "type" ~> ("dl" :: Text)
        , "items" ~>
            [ mapFromList [ "dt" ~> dt, "dd" ~> dd ] :: HashMap Text (GVal m)
            | (dt, dd) <- pairs
            ]
        ]
blockProperties (Header level (id, classes, attrs) _) =
    mapFromList
        [ "type" ~> ("h" ++ show level :: String)
        , "id" ~> id
        , "classes" ~> classes
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
blockProperties HorizontalRule = mapFromList ["type" ~> ("hr" :: Text)]
blockProperties (Table caption alignments widths headers rows) =
    mapFromList
        [ "type" ~> ("table" :: Text)
        , "caption" ~> caption
        , "columns" ~>
            [ mapFromList
                [ "align" ~> alignment
                , "width" ~> fromFloatDigits width
                , "header" ~> header
                ] :: HashMap Text (GVal m)
            | (alignment, width, header)
            <- zip3 alignments widths headers
            ]
        , "rows" ~> rows
        ]
blockProperties (Div (id, classes, attrs) _) =
    mapFromList
        [ "type" ~> ("div" :: Text)
        , "id" ~> id
        , "classes" ~> classes
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
blockProperties Null = mapFromList []

instance ToGVal m Alignment where
    toGVal AlignLeft = toGVal ("left" :: Text)
    toGVal AlignRight = toGVal ("right" :: Text)
    toGVal AlignCenter = toGVal ("center" :: Text)
    toGVal AlignDefault = def

instance ToGVal m Inline where
    toGVal inline =
        let pandoc = Pandoc nullMeta [Plain [inline]]
            listItems =
                (map toGVal $ (query (:[]) inline :: [Inline])) ++
                (map toGVal $ (query (:[]) inline :: [Block]))
        in def { asList = Just listItems
               , asDictItems = Just $ mapToList (inlineProperties inline)
               , asLookup = Just $ \key -> lookup key (inlineProperties inline)
               , asHtml = unsafeRawHtml . pack . writeHtmlString writerOptions $ pandoc
               , asText = pack . writePlain writerOptions $ pandoc
               , asBoolean = True
               , asNumber = Nothing
               , asFunction = Nothing
               , Ginger.length = Just (ClassyPrelude.length listItems)
               , isNull = False
               }

inlineProperties :: forall m. Inline -> HashMap Text (GVal m)
inlineProperties (Str _) = mapFromList ["type" ~> ("str" :: Text)]
inlineProperties (Emph _) = mapFromList ["type" ~> ("em" :: Text)]
inlineProperties (Strong _) = mapFromList ["type" ~> ("strong" :: Text)]
inlineProperties (Strikeout _) = mapFromList ["type" ~> ("strikeout" :: Text)]
inlineProperties (Superscript _) = mapFromList ["type" ~> ("superscript" :: Text)]
inlineProperties (Subscript _) = mapFromList ["type" ~> ("subscript" :: Text)]
inlineProperties (SmallCaps _) = mapFromList ["type" ~> ("smallCaps" :: Text)]
inlineProperties (Quoted quoteType _) = mapFromList ["type" ~> ("quoted" :: Text)]
inlineProperties (Cite citations _) =
    mapFromList
        [ "type" ~> ("cite" :: Text)
        , "citations" ~> citations
        ]
inlineProperties (Code (id, classes, attrs) _) =
    mapFromList
        [ "type" ~> ("code" :: Text)
        , "id" ~> id
        , "classes" ~> classes
        , "attrs" ~> attrs
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
inlineProperties Space = mapFromList ["type" ~> ("space" :: Text)]
inlineProperties LineBreak = mapFromList ["type" ~> ("br" :: Text)]
inlineProperties (Math mathType _) = mapFromList ["type" ~> ("math" :: Text)]
inlineProperties (RawInline fmt _) = mapFromList ["type" ~> ("rawInline" :: Text)]
inlineProperties (Link (id, classes, attrs) _ target) =
    mapFromList
        [ "type" ~> ("link" :: Text)
        , "id" ~> id
        , "classes" ~> classes
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
inlineProperties (Image (id, classes, attrs) _ target) =
    mapFromList
        [ "type" ~> ("image" :: Text)
        , "id" ~> id
        , "classes" ~> classes
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
inlineProperties (Note _) = mapFromList ["type" ~> ("note" :: Text)]
inlineProperties (Span (id, classes, attrs) _) =
    mapFromList
        [ "type" ~> ("span" :: Text)
        , "id" ~> id
        , "classes" ~> classes
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]

instance ToGVal m Citation where
    toGVal c =
        dict [ "id" ~> citationId c
             , "prefix" ~> citationPrefix c
             , "suffix" ~> citationSuffix c
             , "mode" ~> citationMode c
             , "noteNum" ~> citationNoteNum c
             , "hash" ~> citationHash c
             ]

instance ToGVal m CitationMode where
    toGVal = toGVal . show
