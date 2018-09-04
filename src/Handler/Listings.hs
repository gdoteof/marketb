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

import qualified Data.ByteString.Char8 as C

import Data.UUID.V4 as U4 (nextRandom)
import Data.UUID as U (toText)

import Data.Text (splitOn)

import qualified Data.List as DL (head, last)

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
    imagesOnDisk <- sequence $  map img2disk (lqimages listingReq)
    newId  <- runDB $ insert $ Listing userId (lqname listingReq) (Just imagesOnDisk) (lqprice listingReq) now
    return $ object ["newId" .= newId]
    where
        img2disk :: Text -> Handler FilePath
        img2disk s64 = do
            let extension =  DL.head $ splitOn ";" $ DL.last $ splitOn "/" $ DL.head $ splitOn "," s64
            let dataPart  = C.pack $ unpack $ DL.last $ splitOn "," s64
            let dataFile  = case B64.decode  dataPart of
                                Left error -> B64.encode "error"
                                Right bs -> bs
            uuid <- liftIO $ getUUID
            let newFileName = uuid ++ "." ++ extension
            liftIO $ print newFileName
            liftIO $ print $ take 30 $ unpack $ DL.last $ splitOn "," s64
            ret <- liftIO $ B.writeFile ("./static/img/" ++ unpack newFileName) dataFile
            return $ unpack newFileName
            where
                getUUID :: IO Text
                getUUID = do
                    seed <- U4.nextRandom
                    return $ U.toText seed
