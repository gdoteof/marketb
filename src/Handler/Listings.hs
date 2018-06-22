{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Listings where

import Import

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))



data ListingReq = ListingReq 
    { lqname     :: Text
    , lqprice    :: Double
    } deriving Show


type FullResponse = [ListingResp]

instance FromJSON ListingReq where
    parseJSON (Object o) = ListingReq
        <$> o .: "name"
        <*> o .: "price"

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
                )

    let cleanListings = map (\(n,p,u) -> object 
                                [ "name"  .= (E.unValue n :: Text)
                                , "price" .= (E.unValue p :: Double)
                                , "user"  .= fromMaybe "Anonymous" (E.unValue u :: Maybe Text)
                                ]) listings

    return $ object ["listings" .= cleanListings ]


postListingsR :: Handler Value
postListingsR = do
    (userId, _) <- requireAuthPair
    listingReq <- requireJsonBody :: Handler ListingReq
    liftIO $ putStrLn $ pack $ show $ listingReq
    now    <- liftIO $ getCurrentTime
    newId  <- runDB $ insert $ Listing userId (lqname listingReq) (lqprice listingReq) now
    return $ object ["newId" .= newId]
