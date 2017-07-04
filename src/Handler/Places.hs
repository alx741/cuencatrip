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

data PlaceData = PlaceData
    { placeName :: String
    , placeLat :: Float
    , placeLng :: Float
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

toCoordinate :: Parser Coordinate
toCoordinate = do
    _ <- string "POINT("
    lng <- ap sign fractional
    spaces
    lat <- ap sign fractional
    return $ Coordinate lat lng

getPlacesR :: Place -> Handler Value
getPlacesR _ = do
    let sql = "select name, ST_AsText(shop.geog) from shop where gid=100"
    runDB $ rawQuery sql [] $$ CL.mapM_ (liftIO . print)
    coordinate <- case parse toCoordinate "" "POINT(-79.0209324 -2.8976525)" of
            Left err -> notFound
            Right coor -> return coor
    return $ toJSON coordinate


getCuencaR :: Handler Html
getCuencaR = do
    mlat <- lookupGetParam "lat"
    mlong <- lookupGetParam "long"

    lat <- case mlat of
            Nothing -> redirect HomeR
            Just x -> return $ rawJS x

    long <- case mlong of
            Nothing -> redirect HomeR
            Just x -> return $ rawJS x

    defaultLayout $ do
        setTitle "CuencaTrip"
        $(widgetFile "places")
