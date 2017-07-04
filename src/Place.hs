{-# LANGUAGE OverloadedStrings #-}

module Place where

import ClassyPrelude
import qualified Data.Text as T
import Yesod.Core.Dispatch

data Place
    = Bar
    | Cafe
    | Hotel
    | Restaurant
    | Museum
    | Shop
    deriving (Show, Read, Eq)

class IsOnDB a where
    isOnTable :: a -> Text
    isOnAttribute :: a -> Text
    filter :: a -> Text

instance IsOnDB Place where
    isOnTable Bar = "amenity"
    isOnTable Cafe = "amenity"
    isOnTable Hotel = "tourism"
    isOnTable Restaurant = "amenity"
    isOnTable Museum = "tourism"
    isOnTable Shop = "shop"

    isOnAttribute Bar = isOnTable Bar
    isOnAttribute Cafe = isOnTable Cafe
    isOnAttribute Hotel = isOnTable Hotel
    isOnAttribute Restaurant = isOnTable Restaurant
    isOnAttribute Museum = isOnTable Museum
    isOnAttribute Shop = isOnTable Shop

    filter Bar = toSQLOred Bar filters
        where filters = ["bar"]

    filter Cafe = toSQLOred Cafe filters
        where filters = ["cafe"]

    filter Hotel = toSQLOred Hotel filters
        where filters =
                [ "hotel"
                , "hostel"
                , "motel"
                ]

    filter Restaurant = toSQLOred Restaurant filters
        where filters =
                [ "restaurant"
                , "ice_cream"
                , "fast_food"
                ]

    filter Museum = toSQLOred Museum filters
        where filters =
                [ "museum"
                , "attraction"
                , "artwork"
                ]

    filter Shop = toSQLOred Shop filters
        where filters =
                [ "supermarket"
                , "books"
                , "boutique"
                , "clothes"
                , "convenience"
                , "shoes"
                , "variety_store"
                ]

toSQLOred :: (IsOnDB a) => a -> [Text] -> Text
toSQLOred p tx = T.drop 3 $ T.concat
    $ fmap ((flip T.snoc) '\'')
    $ fmap (T.append ored) tx
    where ored = T.concat [" OR ", isOnTable p, ".", isOnAttribute p, " = '"]

instance PathPiece Place where
    fromPathPiece t = readMay t
    toPathPiece p = pack $ show p
