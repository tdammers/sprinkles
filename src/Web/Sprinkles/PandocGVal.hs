{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Web.Sprinkles.PandocGVal
where

import ClassyPrelude hiding (asText, asList)
import Text.Ginger as Ginger (GVal (..), ToGVal (..), dict, (~>))
import Text.Ginger.Html (unsafeRawHtml)
import qualified Text.Ginger as Ginger
import qualified Text.Ginger.Run.VM as Ginger
import Text.Pandoc
import Text.Pandoc.Walk (query, walk)
import Data.Default (def)
import Data.Aeson (ToJSON (..))
import Data.Scientific (fromFloatDigits)

writerOptions :: WriterOptions
writerOptions =
    def { writerStandalone = False
        , writerHtml5 = True
        }

gfnWithMediaRoot :: Monad m => Pandoc -> Ginger.Function (Ginger.Run p m h)
gfnWithMediaRoot pandoc args = do
    defMediaroot <- fromMaybe def . Ginger.lookupKey "path" <$> Ginger.getVar "request"
    let extracted =
            Ginger.extractArgsDefL
                [ ("mediaroot", defMediaroot)
                ]
                args
    case extracted of
        Right [mediaroot] -> do
            let pandoc' = relativeUrlPrefix (unpack $ asText mediaroot) pandoc
            return $ toGVal pandoc'
        _ -> return def

gfnWithAppRoot :: Monad m => Pandoc -> Ginger.Function (Ginger.Run p m h)
gfnWithAppRoot pandoc args = do
    defApproot <- Ginger.getVar "approot"
    let extracted =
            Ginger.extractArgsDefL
                [ ("approot", defApproot)
                ]
                args
    case extracted of
        Right [approot] -> do
            let pandoc' = localUrlPrefix (unpack $ asText approot) pandoc
            return $ toGVal pandoc'
        _ -> return def

prefixRelativeUrl :: String -> String -> String
prefixRelativeUrl prefix url
    | "http://" `isPrefixOf` url = url
    | "https://" `isPrefixOf` url = url
    | "/" `isPrefixOf` url = url
    | ":" `isPrefixOf` url = url
    | otherwise = prefix ++ "/" ++ url

prefixLocalUrl :: String -> String -> String
prefixLocalUrl prefix url
    | "http://" `isPrefixOf` url = url
    | "https://" `isPrefixOf` url = url
    | ":" `isPrefixOf` url = url
    | "/" `isPrefixOf` url = prefix ++ url
    | otherwise = url

modifyUrls :: (String -> String) -> Pandoc -> Pandoc
modifyUrls f = walk goInline
    where
        goInline :: Inline -> Inline
        goInline (Image attrs inlines (url, title)) =
            Image attrs (map goInline inlines) (f url, title)
        goInline (Link attrs inlines (url, title)) =
            Link attrs (map goInline inlines) (f url, title)
        goInline x = x

localUrlPrefix :: String -> Pandoc -> Pandoc
localUrlPrefix prefix = modifyUrls (prefixLocalUrl prefix)

relativeUrlPrefix :: String -> Pandoc -> Pandoc
relativeUrlPrefix prefix = modifyUrls (prefixRelativeUrl prefix)

instance Monad m => ToGVal (Ginger.Run p m h) Pandoc where
    toGVal pandoc@(Pandoc meta blocks) =
        def { asList = Just $ map toGVal blocks
            , asDictItems =
                Just
                    [ ( "meta", toGVal meta )
                    , ( "body", toGVal blocks )
                    , ( "withAppRoot"
                      , Ginger.fromFunction . gfnWithAppRoot $ pandoc
                      )
                    , ( "withMediaRoot"
                      , Ginger.fromFunction . gfnWithMediaRoot $ pandoc
                      )
                    ]
            , asLookup = Just $ \case
                            "meta" -> Just (toGVal meta)
                            "body" -> Just (toGVal blocks)
                            "withMediaRoot" -> Just . Ginger.fromFunction . gfnWithMediaRoot $ pandoc
                            "withAppRoot" -> Just . Ginger.fromFunction . gfnWithAppRoot $ pandoc
                            _ -> Nothing
            , asHtml = unsafeRawHtml . pack . writeHtmlString writerOptions $ pandoc
            , asText = unwords . fmap (asText . toGVal) $ blocks
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
            listItems :: [GVal m]
            blockProps :: HashMap Text (GVal m)
            (blockProps, listItems) = blockChildren block
            baseProps = mapFromList ["children" ~> listItems]
            props = baseProps <> blockProps
        in def { asList = Just listItems
               , asDictItems = Just $ mapToList props
               , asLookup = Just $ \key -> lookup key props
               , asHtml = unsafeRawHtml . pack . writeHtmlString writerOptions $ pandoc
               , asText = unwords . fmap ((<> " ") . asText) $ listItems
               , asBoolean = True
               , asNumber = Nothing
               , asFunction = Nothing
               , Ginger.length = Just (ClassyPrelude.length listItems)
               , isNull = False
               }

blockProperties :: forall m. Block -> HashMap Text (GVal m)
blockProperties = fst . blockChildren
blockItems :: forall m. Block -> [GVal m]
blockItems = snd . blockChildren

blockChildren :: forall m. Block -> (HashMap Text (GVal m), [GVal m])
blockChildren (Plain items) =
    ( mapFromList ["type" ~> ("plain" :: Text)]
    , fmap toGVal items
    )
blockChildren (Para items) =
    ( mapFromList ["type" ~> ("p" :: Text)]
    , fmap toGVal items
    )
blockChildren (CodeBlock (id, classes, attrs) items) =
    ( mapFromList
        [ "type" ~> ("code" :: Text)
        , "id" ~> (pack id :: Text)
        , "classes" ~> (fmap pack classes :: [Text])
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
    , fmap toGVal items
    )
blockChildren (RawBlock (Format fmt) items) =
    ( mapFromList
        [ "type" ~> ("raw" :: Text)
        , "format" ~> fmt
        ]
    , fmap toGVal items
    )
blockChildren (BlockQuote items) =
    ( mapFromList ["type" ~> ("blockquote" :: Text)]
    , fmap toGVal items
    )
blockChildren (OrderedList _ items) =
    ( mapFromList
        [ "type" ~> ("ol" :: Text)
        , "items" ~> items
        ]
    , fmap toGVal items
    )
blockChildren (BulletList items) =
    ( mapFromList
        [ "type" ~> ("ul" :: Text)
        , "items" ~> items
        ]
    , fmap toGVal items
    )
blockChildren (DefinitionList pairs) =
    ( mapFromList
        [ "type" ~> ("dl" :: Text)
        , "items" ~>
            [ mapFromList [ "dt" ~> dt, "dd" ~> dd ] :: HashMap Text (GVal m)
            | (dt, dd) <- pairs
            ]
        ]
    , fmap toGVal pairs
    )
blockChildren (Header level (id, classes, attrs) items) =
    ( mapFromList
        [ "type" ~> ("h" <> tshow level :: Text)
        , "id" ~> (pack id :: Text)
        , "classes" ~> (fmap pack classes :: [Text])
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
    , fmap toGVal items
    )
blockChildren HorizontalRule =
    ( mapFromList ["type" ~> ("hr" :: Text)]
    , []
    )
blockChildren (Table caption alignments widths headers rows) =
    ( mapFromList
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
    , fmap toGVal rows :: [GVal m]
    )
blockChildren (Div (id, classes, attrs) items) =
    ( mapFromList
        [ "type" ~> ("div" :: Text)
        , "id" ~> (pack id :: Text)
        , "classes" ~> (fmap pack classes :: [Text])
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
    , fmap toGVal items
    )
blockChildren Null = (mapFromList [], [])

instance ToGVal m Alignment where
    toGVal AlignLeft = toGVal ("left" :: Text)
    toGVal AlignRight = toGVal ("right" :: Text)
    toGVal AlignCenter = toGVal ("center" :: Text)
    toGVal AlignDefault = def

instance ToGVal m Inline where
    toGVal inline =
        let pandoc = Pandoc nullMeta [Plain [inline]]
            listItems :: [GVal m]
            inlineProps :: HashMap Text (GVal m)
            (inlineProps, listItems) = inlineChildren inline
            baseProps = mapFromList ["children" ~> listItems]
            props = baseProps <> inlineProps
        in def { asList = Just listItems
               , asDictItems = Just $ mapToList props
               , asLookup = Just $ \key -> lookup key props
               , asHtml = unsafeRawHtml . pack . writeHtmlString writerOptions $ pandoc
               , asText = unwords . fmap asText $ listItems
               , asBoolean = True
               , asNumber = Nothing
               , asFunction = Nothing
               , Ginger.length = Just (ClassyPrelude.length listItems)
               , isNull = False
               }

inlineChildren :: forall m. Inline -> (HashMap Text (GVal m), [GVal m])
inlineChildren (Str str) =
    ( mapFromList ["type" ~> ("str" :: Text)]
    , [toGVal . (pack :: String -> Text) $ str] :: [GVal m]
    )
inlineChildren (Emph items) =
    ( mapFromList ["type" ~> ("em" :: Text)]
    , fmap toGVal items
    )
inlineChildren (Strong items) =
    ( mapFromList ["type" ~> ("strong" :: Text)]
    , fmap toGVal items
    )
inlineChildren (Strikeout items) =
    ( mapFromList ["type" ~> ("strikeout" :: Text)]
    , fmap toGVal items
    )
inlineChildren (Superscript items) =
    ( mapFromList ["type" ~> ("superscript" :: Text)]
    , fmap toGVal items
    )
inlineChildren (Subscript items) =
    ( mapFromList ["type" ~> ("subscript" :: Text)]
    , fmap toGVal items
    )
inlineChildren (SmallCaps items) =
    ( mapFromList ["type" ~> ("smallCaps" :: Text)]
    , fmap toGVal items
    )
inlineChildren (Quoted quoteType items) =
    ( mapFromList ["type" ~> ("quoted" :: Text)]
    , fmap toGVal items
    )
inlineChildren (Cite citations items) =
    ( mapFromList
        [ "type" ~> ("cite" :: Text)
        , "citations" ~> citations
        ]
    , fmap toGVal items
    )
inlineChildren (Code (id, classes, attrs) code) =
    ( mapFromList
        [ "type" ~> ("code" :: Text)
        , "id" ~> (pack id :: Text)
        , "classes" ~> (fmap pack classes :: [Text])
        , "attrs" ~> attrs
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
    , [toGVal (pack code :: Text)]
    )
inlineChildren Space = (mapFromList ["type" ~> ("space" :: Text)], [toGVal (" " :: Text)])
inlineChildren SoftBreak = (mapFromList ["type" ~> ("sbr" :: Text)], [toGVal (" " :: Text)])
inlineChildren LineBreak = (mapFromList ["type" ~> ("br" :: Text)], [toGVal (" " :: Text)])
inlineChildren (Math mathType src) = (mapFromList ["type" ~> ("math" :: Text)], [toGVal (pack src:: Text)])
inlineChildren (RawInline fmt src) = (mapFromList ["type" ~> ("rawInline" :: Text)], [toGVal (pack src:: Text)])
inlineChildren (Link (id, classes, attrs) items target) =
    ( mapFromList
        [ "type" ~> ("link" :: Text)
        , "id" ~> (pack id :: Text)
        , "classes" ~> (fmap pack classes :: [Text])
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
    , fmap toGVal items
    )
inlineChildren (Image (id, classes, attrs) items target) =
    ( mapFromList
        [ "type" ~> ("image" :: Text)
        , "id" ~> (pack id :: Text)
        , "classes" ~> (fmap pack classes :: [Text])
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
    , fmap toGVal items
    )
inlineChildren (Note items) = (mapFromList ["type" ~> ("note" :: Text)], fmap toGVal items)
inlineChildren (Span (id, classes, attrs) items) =
    ( mapFromList
        [ "type" ~> ("span" :: Text)
        , "id" ~> (pack id :: Text)
        , "classes" ~> (fmap pack classes :: [Text])
        , ("attrs", dict [ pack t ~> v | (t, v) <- attrs ])
        ]
    , fmap toGVal items
    )

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
