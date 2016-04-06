{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
module Web.Templar.Backends
where

import ClassyPrelude
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Data.Yaml as YAML
import qualified Network.HTTP as HTTP
import Network.Mime
            ( MimeType
            , MimeMap
            , defaultMimeLookup
            , defaultMimeMap
            , mimeByExt
            , defaultMimeType
            , FileName
            )
import Network.URI (parseURI)

mimeMap :: MimeMap
mimeMap =
    defaultMimeMap <>
    mapFromList
        [ ("yml", "application/x-yaml")
        , ("yaml", "application/x-yaml")
        ]

mimeLookup :: FileName -> MimeType
mimeLookup = mimeByExt mimeMap defaultMimeType

loadBackendData :: String -> IO JSON.Value
loadBackendData backendURLStr = do
    (mimeType, responseBody) <- fetchBackendData backendURLStr
    parseBackendData mimeType responseBody

fetchBackendData :: String -> IO (MimeType, LByteString)
fetchBackendData backendURLStr = do
    let protocol = takeWhile (/= ':') backendURLStr
    case protocol of
        "http" -> fetchBackendDataHTTP backendURLStr
        "https" -> fetchBackendDataHTTP backendURLStr
        "file" -> fetchBackendDataFile (drop 7 backendURLStr)
        x -> fail $ "Unknown protocol: " <> show x

fetchBackendDataFile :: String -> IO (MimeType, LByteString)
fetchBackendDataFile filename = do
    let mimeType = mimeLookup . pack $ filename
    contents <- readFile filename
    return (mimeType, contents)

fetchBackendDataHTTP :: String -> IO (MimeType, LByteString)
fetchBackendDataHTTP backendURLStr = do
    backendURL <- maybe
        (fail $ "Invalid backend URL: " ++ backendURLStr)
        return
        (parseURI backendURLStr)
    let backendRequest =
            HTTP.Request
                backendURL
                HTTP.GET
                []
                ""
    backendResponse <- HTTP.simpleHTTP backendRequest
    backendBody <- HTTP.getResponseBody backendResponse
    headers <- case backendResponse of
                    Left err -> fail (show err)
                    Right resp -> return $ HTTP.getHeaders resp
    let backendType = encodeUtf8 . pack . fromMaybe "text/plain" . lookupHeader HTTP.HdrContentType $ headers
    return (backendType, backendBody)

lookupHeader :: HTTP.HeaderName -> [HTTP.Header] -> Maybe String
lookupHeader name headers =
    headMay [ v | HTTP.Header n v <- headers, n == name ]

parseBackendData :: Monad m => MimeType -> LByteString -> m JSON.Value
parseBackendData "application/json" = parseJSONResponse
parseBackendData "text/json" = parseJSONResponse
parseBackendData "application/x-yaml" = parseYamlResponse
parseBackendData "application/yaml" = parseYamlResponse
parseBackendData "text/yaml" = parseYamlResponse
parseBackendData "text/x-yaml" = parseYamlResponse
parseBackendData m = fail $ "Unknown or invalid content type: " <> show m

parseJSONResponse :: Monad m => LByteString -> m JSON.Value
parseJSONResponse jsonSrc =
    case JSON.eitherDecode jsonSrc of
        Left err -> fail $ err ++ "\n" ++ show jsonSrc
        Right json -> return (json :: JSON.Value)

parseYamlResponse :: Monad m => LByteString -> m JSON.Value
parseYamlResponse yamlSrc =
    case YAML.decodeEither (toStrict yamlSrc) of
        Left err -> fail $ err ++ "\n" ++ show yamlSrc
        Right json -> return (json :: JSON.Value)
