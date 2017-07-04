module Place where

import ClassyPrelude
import Yesod.Core.Dispatch

data Place
    = Bar
    | Cafe
    | Hotel
    | Restaurant
    | Museum
    | Shop
    deriving (Show, Read, Eq)

instance PathPiece Place where
    fromPathPiece t = readMay t
    toPathPiece p = pack $ show p
