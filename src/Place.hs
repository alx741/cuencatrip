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
    isOnTable Shop = "shop"

    isOnAttribute Bar = isOnTable Bar
    isOnAttribute Cafe = isOnTable Cafe
    isOnAttribute Shop = isOnTable Shop

    filter Bar = toSQLOred Bar filters
        where filters = ["bar"]

    filter Cafe = toSQLOred Cafe filters
        where filters = ["cafe"]

    filter Shop = toSQLOred Shop filters
        where filters =
                ["supermarket"
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
