{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Sprinkles.Handlers.Static
( handleStaticTarget
)
where

import ClassyPrelude
import Web.Sprinkles.Backends
import qualified Network.Wai as Wai
import Web.Sprinkles.Logger as Logger
import Web.Sprinkles.Project
import Web.Sprinkles.ProjectConfig
import Web.Sprinkles.Handlers.Common
import Network.HTTP.Types
       (Status, status200, status206, status302, status400, status404, status500)
import Web.Sprinkles.Backends.Loader.Type
       (PostBodySource (..), pbsFromRequest, pbsInvalid)
import Web.Sprinkles.Backends.Data
       (RawBytes (..))
import Data.AList (AList)
import qualified Data.AList as AList
import Data.Char (isSpace)
import Text.Read (readMaybe)
import qualified Data.Text as Text
import Text.Printf (printf)

handleStaticTarget :: Maybe Text -> ContextualHandler
handleStaticTarget childPathMay
                   backendData
                   project
                   request
                   respond = do
    backendItemBase <- case lookup "file" backendData of
        Nothing -> throwM NotFoundException
        Just NotFound -> throwM NotFoundException
        Just (SingleItem item) -> return item
        Just (MultiItem []) -> throwM NotFoundException
        Just (MultiItem (x:xs)) -> return x
    backendItem <- case childPathMay of
        Nothing -> return backendItemBase
        Just path -> case lookup path (bdChildren backendItemBase) of
            Nothing -> throwM NotFoundException
            Just item -> return item
    let responseRangeMay = do
            requestRangeHeader <- lookup "Range" $ Wai.requestHeaders request
            parseRequestRangeHeader $ decodeUtf8 requestRangeHeader
    case responseRangeMay of
        Nothing -> do
            responseBytes <- rawToLBS (bdRaw backendItem)
            respond $ Wai.responseLBS
                status200
                    [ ("Content-type", bmMimeType . bdMeta $ backendItem)
                    , ("Accept-ranges", "bytes")
                    ]
                responseBytes
        Just (from, to) -> do
            responseLength <- rbLength . bdRaw $ backendItem
            let from' = (min from responseLength)
                to' = (min to responseLength)
                rangeHeaderValue = encodeUtf8 . Text.pack $
                    printf "%d-%d/%d"
                        from'
                        to'
                        responseLength
            responseBytes <- rbGetRange (bdRaw backendItem) from' to'
            respond $ Wai.responseLBS
                status206
                    [ ("Content-type", bmMimeType . bdMeta $ backendItem)
                    , ("Accept-ranges", "bytes")
                    , ("Range", rangeHeaderValue)
                    ]
                responseBytes


parseRequestRangeHeader :: Text -> Maybe (Integer, Integer)
parseRequestRangeHeader src = do
    let src' :: Text
        src' = filter (not . isSpace) src
    when (not $ "bytes=" `isPrefixOf` src')
        Nothing
    let src'' :: Text
        src'' = drop (length ("bytes=" :: Text)) src'
    case Text.splitOn "-" src'' of
        [fromStr, toStr] -> do
            from <- readMaybe $ Text.unpack fromStr
            to <- readMaybe $ Text.unpack toStr
            return (from, to)
        _ -> Nothing
