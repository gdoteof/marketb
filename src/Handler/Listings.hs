{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Listings where

import Import

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64



data ListingReq = ListingReq 
    { lqname     :: Text
    , lqprice    :: Double
    , lqimages   :: [Text]
    } deriving Show


type FullResponse = [ListingResp]

instance FromJSON ListingReq where
    parseJSON (Object o) = ListingReq
        <$> o .: "name"
        <*> o .: "price"
        <*> o .: "images"

    parseJSON _ = mzero

data ListingResp = ListingResp
    { lrname  :: Text
    , lrprice :: Double
    , lruserName :: Text
    }


getListingsR :: Handler Value
getListingsR = do
    listings <- runDB
		$ E.select
		$ E.from $ \(listing `E.InnerJoin` user) -> do
            E.on $ listing ^. ListingUserId E.==. user ^. UserId
            return
                (   listing ^. ListingName
                ,   listing ^. ListingPrice
                ,   user    ^. UserIdent
                ,   listing ^. ListingId
                )

    let cleanListings = map (\(n,p,u,id) -> object 
                                [ "name"  .= (E.unValue n :: Text)
                                , "price" .= (E.unValue p :: Double)
                                , "user"  .= fromMaybe "Anonymous" (E.unValue u :: Maybe Text)
                                , "key"   .= (E.unValue id)
                                ]) listings

    return $ object ["listings" .= cleanListings ]


postListingsR :: Handler Value
postListingsR = do
    (userId, _) <- requireAuthPair
    listingReq <- requireJsonBody :: Handler ListingReq
    now    <- liftIO $ getCurrentTime
    images <- sequence $  map img2disk (lqimages listingReq)
    newId  <- runDB $ insert $ Listing userId (lqname listingReq) (Just images) (lqprice listingReq) now
    return $ object ["newId" .= newId]
    where
        img2disk :: Text -> Handler Photo
        img2disk s64 = do
            let b64 = encodeUtf8 s64
            ret <- liftIO $ B.writeFile ("./" ++ undefined) b64
            return ret
            undefined
