{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Places where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Database.Persist.Sql
import Data.Conduit
import Prelude (read)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Token
import Text.ParserCombinators.Parsec.Numeric
import Place

radius :: Int
radius = 1000

data PlaceData = PlaceData
    { placeId :: Int64
    , placeName :: Text
    , placeCoor :: Coordinate
    , placeRaters :: Int64
    , placeRating :: Float
    } deriving (Generic, Show)

instance ToJSON PlaceData
instance FromJSON PlaceData


data Coordinate = Coordinate
    { coorLat :: Double
    , coorLng :: Double
    } deriving (Generic, Show, Read, Eq)

instance ToJSON Coordinate
instance FromJSON Coordinate

getRatePlaceR :: Place -> Int64 -> Int -> Handler Value
getRatePlaceR place gid rating = do
    -- Obtain place
    let getPlaceSql = "select gid, name, raters, rating, ST_AsText("
            ++ geog place ++ ")  from " ++ isOnTable place
            ++ " where gid=" ++ pack (show gid)

    queryResult <- runDB $ rawQuery getPlaceSql [] $$ CL.consume

    placeData <- case headMay $ fmap getPlaceData queryResult of
            Nothing -> redirect HomeR
            Just x -> return $ x


    -- Bump place rating
    let bumpedPlace = placeData { placeRating =
        rate (placeRating placeData) (placeRaters placeData) rating }

    let updatePlaceSql = "update " ++ isOnTable place ++ " set rating="
            ++ pack (show (placeRating bumpedPlace))
            ++ ", raters=" ++ pack (show (placeRaters placeData + 1))
            ++ " where gid=" ++ pack (show gid)

    _ <-  runDB $ rawExecute updatePlaceSql []

    return $ toJSON ("done" :: Text)


rate :: Float -> Int64 -> Int -> Float
rate currentRating raters raiting =
    ((raters' * currentRating) + raiting') / (raters' + 1.0)
    where
        raters' = fromIntegral raters
        raiting' = fromIntegral raiting

getPlacesR :: Place -> Handler Value
getPlacesR place = do
    (lng, lat) <- coordParam
    let sql = "select gid, name, raters, rating, ST_AsText("
            ++ geog place ++ ") from " ++ isOnTable place
            ++ " where ST_Distance(" ++ geog place ++ ", ST_Point("
            ++ lng ++ ", " ++ lat ++ ")) < " ++ pack (show radius)
            ++ " AND " ++ Place.filter place
    queryResult <- runDB $ rawQuery sql [] $$ CL.consume
    return $ toJSON $ fmap getPlaceData queryResult

getPlaceData :: [PersistValue] -> PlaceData
getPlaceData
    ( PersistInt64 gid
    : PersistText name
    : PersistInt64 raters
    : PersistRational rating
    : PersistText coor
    : _
    ) =
    PlaceData gid name (parseCoordinate coor) raters $ fromRational rating
getPlaceData _ = error "Malformed GeoDB row"

parseCoordinate :: Text -> Coordinate
parseCoordinate coorText =
    case parse coordinateParser "" coorText of
            Left e -> error $ show e
            Right coor -> coor

coordinateParser :: Parser Coordinate
coordinateParser = do
    _ <- string "POINT("
    lng <- ap sign fractional
    spaces
    lat <- ap sign fractional
    return $ Coordinate lat lng

getCuencaR :: Handler Html
getCuencaR = do
    (lng, lat) <- coordParam >>= (\(a, b) -> return $ (rawJS a, rawJS b))
    defaultLayout $ do
        setTitle "CuencaTrip"
        $(widgetFile "places")

coordParam :: Handler (Text, Text)
coordParam = do
    mlat <- lookupGetParam "lat"
    mlng <- lookupGetParam "lng"

    lat <- case mlat of
            Nothing -> redirect HomeR
            Just x -> return x

    lng <- case mlng of
            Nothing -> redirect HomeR
            Just x -> return x

    return (lng, lat)
