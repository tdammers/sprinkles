{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

module Web.Sprinkles.Databases
where

import ClassyPrelude
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import qualified Database.HDBC as HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC.MySQL (connectMySQL, MySQLConnectInfo (..))
import qualified Data.Serialize as Cereal
import Data.Serialize (Serialize)

data DSN = DSN { dsnDriver :: SqlDriver, dsnDetails :: Text }
    deriving (Show, Generic)

type instance Element DSN = Text

instance MonoFunctor DSN where
    omap f (DSN driver details) = DSN driver (f details)

data SqlDriver = SqliteDriver
               | PostgreSQLDriver
               | MySQLDriver
               deriving (Show, Generic)

instance Serialize SqlDriver where
    put SqliteDriver = Cereal.put ("sqlt" :: String)
    put PostgreSQLDriver = Cereal.put ("pg" :: String)
    put MySQLDriver = Cereal.put ("my" :: String)
    get = do
        str <- Cereal.get
        case str :: String of
            "sqlt" -> return SqliteDriver
            "pg" -> return PostgreSQLDriver
            "my" -> return MySQLDriver
            x -> fail $ "Invalid database driver: " <> show x

sqlDriverID :: SqlDriver -> Text
sqlDriverID SqliteDriver = "sqlite"
sqlDriverID PostgreSQLDriver = "postgres"
sqlDriverID MySQLDriver = "mysql"

sqlDriverFromID :: Text -> Maybe SqlDriver
sqlDriverFromID "sqlite" = Just SqliteDriver
sqlDriverFromID "sqlite3" = Just SqliteDriver
sqlDriverFromID "pg" = Just PostgreSQLDriver
sqlDriverFromID "pgsql" = Just PostgreSQLDriver
sqlDriverFromID "postgres" = Just PostgreSQLDriver
sqlDriverFromID "postgresql" = Just PostgreSQLDriver
sqlDriverFromID "my" = Just MySQLDriver
sqlDriverFromID "mysql" = Just MySQLDriver
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
        MySQLDriver -> do
            info <- parseMysqlConnectInfo details
            HDBC.ConnWrapper <$> connectMySQL info

parseMysqlConnectInfo :: Monad m => Text -> m MySQLConnectInfo
parseMysqlConnectInfo details = do
    MySQLConnectInfo
        <$> getStrDef "localhost" "host"
        <*> getStrDef "" "user"
        <*> getStrDef "" "password"
        <*> getStrDef "" "database"
        <*> getIntDef 3306 "port"
        <*> getStrDef "" "socket"
        <*> getStr "group"
    where
        getStr :: Monad m => Text -> m (Maybe String)
        getStr key =
            maybe
                (return Nothing)
                (fmap Just . parseStr)
                (lookup key dict)

        getStrDef :: Monad m => String -> Text -> m String
        getStrDef d = fmap (fromMaybe d) . getStr

        getInt :: Monad m => Text -> m (Maybe Int)
        getInt key =
            maybe
                (return Nothing)
                (fmap Just . parseInt)
                (lookup key dict)

        getIntDef :: Monad m => Int -> Text -> m Int
        getIntDef d = fmap (fromMaybe d) . getInt

        dict :: HashMap Text Text
        dict = mapFromList . catMaybes . map parsePair . splitElem ';' $ details

        parsePair :: Text -> Maybe (Text, Text)
        parsePair src =
            case break (== '=') src of
                (key, "") -> Nothing
                (key, value) -> Just (toLower key, drop 1 value)

        parseStr :: Monad m => Text -> m String
        parseStr = return . unpack

        parseInt :: Monad m => Text -> m Int
        parseInt = maybe (fail "Invalid number") return . readMay
