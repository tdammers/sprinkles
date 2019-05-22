{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE CPP #-}

module Web.Sprinkles.Databases
where

import Web.Sprinkles.Prelude
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import qualified Database.HDBC as HDBC

import qualified Data.Serialize as Cereal
import Data.Serialize (Serialize)
import Data.Expandable

#if FEATURE_POSTGRES
import Database.HDBC.PostgreSQL (connectPostgreSQL)
#endif

#if FEATURE_SQLITE
import Database.HDBC.Sqlite3 (connectSqlite3)
#endif

#if FEATURE_MYSQL
import Database.HDBC.MySQL (connectMySQL, MySQLConnectInfo (..))
#endif

data DSN = DSN { dsnDriver :: SqlDriver, dsnDetails :: Text }
    deriving (Show, Generic)

data ResultSetMode
    = ResultsMerge
    | ResultsNth Int
    | ResultsLast
    deriving (Show, Generic)

instance FromJSON ResultSetMode where
    parseJSON = \case
        String "merge" -> return ResultsMerge
        String "first" -> return $ ResultsNth 0
        String "last" -> return ResultsLast
        String x -> fail $ "Invalid result set mode " ++ show x
        Number i -> if i >= 0
                        then return (ResultsNth . round $ i)
                        else fail $ "Invalid result set index " ++ show i
        x -> fail $ "Expected integer or string for result set mode, got " ++ show x

instance Serialize ResultSetMode where

instance ExpandableM Text DSN where
    expandM f (DSN driver details) = DSN driver <$> f details

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
#if FEATURE_SQLITE
            "sqlt" -> return SqliteDriver
#else
            "sqlt" -> fail $ "SQLite support disabled"
#endif

#if FEATURE_POSTGRES
            "pg" -> return PostgreSQLDriver
#else
            "pg" -> fail $ "PostgreSQL support disabled"
#endif

#if FEATURE_MYSQL
            "my" -> return MySQLDriver
#else
            "my" -> fail $ "MySQL support disabled"
#endif

            x -> fail $ "Invalid database driver: " <> show x

sqlDriverID :: SqlDriver -> Text
sqlDriverID SqliteDriver = "sqlite"
sqlDriverID PostgreSQLDriver = "postgres"
sqlDriverID MySQLDriver = "mysql"

sqlDriverFromID :: Text -> Either String SqlDriver

#if FEATURE_SQLITE
sqlDriverFromID "sqlite" = Right SqliteDriver
sqlDriverFromID "sqlite3" = Right SqliteDriver
#else
sqlDriverFromID "sqlite" = Left "SQLite support disabled"
sqlDriverFromID "sqlite3" = Left "SQLite support disabled"
#endif

#if FEATURE_POSTGRES
sqlDriverFromID "pg" = Right PostgreSQLDriver
sqlDriverFromID "pgsql" = Right PostgreSQLDriver
sqlDriverFromID "postgres" = Right PostgreSQLDriver
sqlDriverFromID "postgresql" = Right PostgreSQLDriver
#else
sqlDriverFromID "pg" = Left "PostgreSQL support disabled"
sqlDriverFromID "pgsql" = Left "PostgreSQL support disabled"
sqlDriverFromID "postgres" = Left "PostgreSQL support disabled"
sqlDriverFromID "postgresql" = Left "PostgreSQL support disabled"
#endif
#if FEATURE_MYSQL
sqlDriverFromID "my" = Right MySQLDriver
sqlDriverFromID "mysql" = Right MySQLDriver
#else
sqlDriverFromID "my" = Left "MySQL support disabled"
sqlDriverFromID "mysql" = Left "MySQL support disabled"
#endif
sqlDriverFromID _ = Left "Invalid SQL driver ID"

instance ToJSON SqlDriver where
    toJSON = toJSON . sqlDriverID

instance FromJSON SqlDriver where
    parseJSON x =
        parseJSON x >>=
            either fail return . sqlDriverFromID

instance Serialize DSN where
    put (DSN driver details) = do
        Cereal.put driver
        Cereal.put ((unpack :: Text -> String) details)
    get = DSN <$> Cereal.get <*> ((pack :: String -> Text) <$> Cereal.get)

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
        SqliteDriver ->
#if FEATURE_SQLITE
          HDBC.ConnWrapper <$> connectSqlite3 (unpack details)
#else
            error "SQLite support disabled"
#endif

        PostgreSQLDriver ->
#if FEATURE_POSTGRES
          HDBC.ConnWrapper <$> connectPostgreSQL (unpack details)
#else
            error "PostgreSQL support disabled"
#endif

        MySQLDriver ->
#if FEATURE_MYSQL
            do
              info <- parseMysqlConnectInfo details
              HDBC.ConnWrapper <$> connectMySQL info
#else
            error "MySQL support disabled"
#endif

#if FEATURE_MYSQL
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
#endif
