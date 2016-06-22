{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

module Web.Templar.Databases
where

import ClassyPrelude
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import qualified Database.HDBC as HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Data.Serialize as Cereal
import Data.Serialize (Serialize)

data DSN = DSN { dsnDriver :: SqlDriver, dsnDetails :: Text }
    deriving (Show, Generic)

data SqlDriver = SqliteDriver
               | PostgreSQLDriver
               deriving (Show, Generic)

instance Serialize SqlDriver where
    put SqliteDriver = Cereal.put ("sqlt" :: String)
    put PostgreSQLDriver = Cereal.put ("pg" :: String)
    get = do
        str <- Cereal.get
        case str :: String of
            "sqlt" -> return SqliteDriver
            "pg" -> return PostgreSQLDriver
            x -> fail $ "Invalid database driver: " <> show x

sqlDriverID :: SqlDriver -> Text
sqlDriverID SqliteDriver = "sqlite"
sqlDriverID PostgreSQLDriver = "postgres"

sqlDriverFromID :: Text -> Maybe SqlDriver
sqlDriverFromID "sqlite" = Just SqliteDriver
sqlDriverFromID "sqlite3" = Just SqliteDriver
sqlDriverFromID "pg" = Just PostgreSQLDriver
sqlDriverFromID "pgsql" = Just PostgreSQLDriver
sqlDriverFromID "postgres" = Just PostgreSQLDriver
sqlDriverFromID "postgresql" = Just PostgreSQLDriver
sqlDriverFromID _ = Nothing

instance ToJSON SqlDriver where
    toJSON = toJSON . sqlDriverID

instance FromJSON SqlDriver where
    parseJSON x =
        parseJSON x >>=
            maybe (fail "Invalid SQL Driver") return . sqlDriverFromID

instance Serialize DSN where
    put (DSN driver details) = do
        Cereal.put driver
        Cereal.put (unpack details)
    get = DSN <$> Cereal.get <*> (pack <$> Cereal.get)

dsnToText :: DSN -> Text
dsnToText (DSN driver details) = sqlDriverID driver <> ":" <> details

instance FromJSON DSN where
    parseJSON (Object obj) = do
        driver <- obj .: "driver"
        details <- obj .: "dsn"
        return $ DSN driver details
    parseJSON _ = fail "Invalid DSN"

withConnection :: DSN -> (HDBC.ConnWrapper -> IO a) -> IO a
withConnection dsn inner = do
    conn <- connect dsn
    inner conn
    
connect :: DSN -> IO HDBC.ConnWrapper
connect (DSN driver details) =
    case driver of
        SqliteDriver -> HDBC.ConnWrapper <$> connectSqlite3 (unpack details)
        PostgreSQLDriver -> HDBC.ConnWrapper <$> connectPostgreSQL (unpack details)
