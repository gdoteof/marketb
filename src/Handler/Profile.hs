{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import

getProfileR :: Handler Value
getProfileR = do
    (_, user) <- requireAuthPair
    return $ object 
        [ "name" .= userIdent user ]



data ProfileReq = ProfileReq {
    profileReqName :: Text
}

instance FromJSON ProfileReq where
    parseJSON (Object o) = do
        ProfileReq <$> (o .: "name")
    parseJSON _ = mzero

putProfileR :: Handler Value
putProfileR = do
    req <- requireJsonBody
    (id, user) <- requireAuthPair
    iz <- runDB $ update id [UserIdent =. (Just . profileReqName) req]
    return $ object 
        [ 
         "id"        .= id
        ]
