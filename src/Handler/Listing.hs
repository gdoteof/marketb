{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Listing where

import Import


getListingR :: ListingId -> Handler Value
getListingR lid = do
    (_, user) <- requireAuthPair
    listing <- runDB $ get404 lid
    return $ object [ "listing" .= listing  ]



