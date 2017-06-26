{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

-- | SQL backend loader
module Web.Sprinkles.Backends.Loader.SqlLoader
( sqlLoader
)
where

import ClassyPrelude
import Web.Sprinkles.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Items (..)
        , reduceItems
        , rawFromLBS
        )
import qualified Database.HDBC as HDBC
import qualified Web.Sprinkles.Databases as DB
import Web.Sprinkles.Databases (DSN (..), sqlDriverFromID)
import Web.Sprinkles.Logger (LogLevel (..))
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Data.Yaml as YAML
import Web.Sprinkles.Backends.Loader.Type
import Data.List.Extra (takeEnd)

sqlLoader :: DSN -> DB.ResultSetMode -> [(Text, [Text])] -> Loader
sqlLoader dsn mode queries writeLog _ fetchMode fetchOrder = do
    resultSets <- DB.withConnection dsn $ \conn -> do
        HDBC.withTransaction conn $ \conn -> do
            forM queries $ \(query, params) -> do
                writeLog Debug $
                    "SQL: QUERY: " <> tshow query <>
                    " ON " <> DB.dsnToText dsn <>
                    " WITH: " <> tshow params
                stmt <- HDBC.prepare conn (unpack query)
                HDBC.execute stmt (map HDBC.toSql params)
                HDBC.fetchAllRowsMap stmt
    return $ mergeResultSets resultSets
    where
        mergeResultSets :: [[Map String HDBC.SqlValue]] -> [BackendSource]
        mergeResultSets [] = []
        mergeResultSets rawRows = case mode of
            DB.ResultsMerge ->
                map mapRow . mconcat $ rawRows
            DB.ResultsNth i ->
                map mapRow . mconcat . drop i . take 1 $ rawRows
            DB.ResultsLast ->
                map mapRow . mconcat . takeEnd 1 $ rawRows

        mapRow :: Map String HDBC.SqlValue -> BackendSource
        mapRow row =
            let json = JSON.encode (fmap (HDBC.fromSql :: HDBC.SqlValue -> Text) row)
                name = maybe "SQL" HDBC.fromSql $
                    lookup "name" row <|>
                    lookup "title" row <|>
                    (headMay . fmap snd . mapToList $ row)
                meta = BackendMeta
                        { bmMimeType = "application/json"
                        , bmMTime = Nothing
                        , bmName = name
                        , bmPath = "SQL"
                        , bmSize = Just . fromIntegral $ length json
                        }
            in BackendSource meta (rawFromLBS json)
