{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Places where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

getPlacesR :: Handler Html
getPlacesR = do
    mlat <- lookupGetParam "lat"
    mlong <- lookupGetParam "long"

    lat <- case mlat of
            Nothing -> redirect HomeR
            Just x -> return x

    long <- case mlong of
            Nothing -> redirect HomeR
            Just x -> return x

    defaultLayout $ do
        $(widgetFile "places")
