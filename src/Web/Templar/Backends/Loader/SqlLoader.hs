{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

-- | SQL backend loader
module Web.Templar.Backends.Loader.SqlLoader
( sqlLoader
)
where

import ClassyPrelude
import Web.Templar.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Items (..)
        , reduceItems
        )
import qualified Database.HDBC as HDBC
import qualified Web.Templar.Databases as DB
import Web.Templar.Databases (DSN (..), sqlDriverFromID)
import Web.Templar.Logger (LogLevel (..))
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Data.Yaml as YAML
import Web.Templar.Backends.Loader.Type

sqlLoader :: DSN -> Text -> [Text] -> Loader
sqlLoader dsn query params writeLog _ fetchMode fetchOrder = do
    rows <- DB.withConnection dsn $ \conn -> do
        HDBC.withTransaction conn $ \conn -> do
            writeLog Debug $
                "SQL: QUERY: " <> tshow query <>
                " ON " <> DB.dsnToText dsn <>
                " WITH: " <> tshow params
            stmt <- HDBC.prepare conn (unpack query)
            HDBC.execute stmt (map HDBC.toSql params)
            HDBC.fetchAllRowsMap stmt
    return $ map mapRow rows
    where
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
            in BackendSource meta json
