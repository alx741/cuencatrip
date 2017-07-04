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
    { placeName :: String
    , placeLat :: Double
    , placeLng :: Double
    , placeRating :: Float
    } deriving (Generic, Show)

instance ToJSON PlaceData
instance FromJSON PlaceData


data Coordinate = Coordinate
    { lat :: Double
    , lng :: Double
    } deriving (Generic, Show, Read, Eq)

instance ToJSON Coordinate
instance FromJSON Coordinate

getPlacesR :: Place -> Handler Value
getPlacesR place = do
    (lng, lat) <- coordParam
    let sql = "select name, raters, rating, ST_AsText("
            ++ geog place ++ ") from " ++ isOnTable place
            ++ " where ST_Distance(" ++ geog place ++ ", ST_Point("
            ++ lng ++ ", " ++ lat ++ ")) < " ++ pack (show radius)
            ++ " AND " ++ Place.filter place
    runDB $ rawQuery sql [] $$ CL.mapM_ (liftIO . print)
    coordinate <- case parse coordinateParser "" "POINT(-79.0209324 -2.8976525)" of
            Left err -> notFound
            Right coor -> return coor
    return $ toJSON coordinate

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
