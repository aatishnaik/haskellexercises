{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Aeson.TwitterBot where
import Data.List as DL
import Control.Monad as CM
import Control.Lens
import Data.Aeson as DA
import Network.Wreq as NW
import Network.Wreq.Types as T
import GHC.Generics
import qualified Data.ByteString.Char8 as DB
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (id)

resGetFollowers :: String
resGetFollowers = "https://api.twitter.com/1.1/friends/list.json"

resunfollow :: String
resunfollow = "https://api.twitter.com/1.1/friendships/destroy.json"

data Follower = Follower {
    id :: Integer,
    name :: String ,
    screen_name :: String,
    followers_count :: Int
}deriving (Show,Eq,Generic)

instance FromJSON Follower where
    parseJSON (Object v) = 
        do
        uid <- v .: "id"
        uname <- v .: "name"
        uscreen_name <- v .: "screen_name"
        ufollowers_count <- v .: "followers_count"
        pure Follower {
            id=uid,
            name=uname,
            screen_name=uscreen_name,
            followers_count=ufollowers_count
        }
    parseJSON _ = pure Follower {}

data FollowerList = FollowerList {
    followerList :: [Follower]
}deriving (Show,Eq,Generic)

instance FromJSON FollowerList where
    parseJSON (Object v) = 
        do
            userlst <- v .: "users"
            pure (FollowerList {followerList = userlst})

    parseJSON _ = pure FollowerList{}

authenticator :: Options
authenticator =
    let 
        authKey = oauth1Auth (DB.pack "FaPdykltzoxz65Tf9TH9ugIe4") (DB.pack "4yxJAXPQMQinymZI6CDgWNhMdLAT68zjucqvlpNeS7glDvSEtc") (DB.pack "976334890915397632-DpsHhYJiTKkQY7ip48JFcqK86xbrsBP") (DB.pack "a6C63vZ8sQmFFgQjm1KnRy4VYPKwRy1dfNcFb5nxliYMH")
        opt = defaults & NW.auth ?~ authKey
    in opt

getFollowers :: String -> IO (Either String FollowerList)
getFollowers userName =
    let urlStr = resGetFollowers++"?cursor=-1&screen_name="++userName++"&skip_status=true&include_user_entities=false"
        userData = getWith authenticator urlStr
    in userData >>= \udata -> pure (eitherDecode (udata ^. responseBody))

--extracts screen name from list of follower objects
getScrNames :: (Either String FollowerList) -> [String]
getScrNames fList = 
    let Right FollowerList {followerList = follArr} = fList
    in DL.map (\Follower{
        id=_,name=_,screen_name=scrname,followers_count=_
    } -> scrname) follArr

getIds :: (Either String FollowerList) -> [Integer]
getIds fList = 
    let Right FollowerList {followerList = follArr} = fList
    in DL.map (\Follower{
        id=usrid,name=_,screen_name=_,followers_count=_
    } -> usrid) follArr

checkFollowers :: [String] -> [String] -> ([String],String)
checkFollowers ufList fList = 
    let
        fol = ufList `intersect` fList
        ufol = DL.foldl' (\arr f-> if(f `notElem` fList) then arr++f++" " else arr) "" ufList
    in (fol,ufol)

getUnfollowerIds :: (Either String FollowerList) -> [String] -> [Integer]
getUnfollowerIds fList ufList= 
    let Right FollowerList {followerList = follArr} = fList
    in DL.foldl' (\arr Follower{
        id=usrid,name=_,screen_name=scrName,followers_count=_
    } -> if (scrName `elem` ufList) then arr++[usrid] else arr) [] follArr
{-followOfFollowers :: IO [IO [String]]
followOfFollowers =
    do
        fObj <- getFollowers "aatishVL"
        fList <- pure (getScrNames fObj)
        fOfF <- pure (DL.map (\n ->(getFollowers n) >>= \x-> pure (getIds x)) fList)
        fOfF >>= \fof -> fof (DL.map () fList)-}
        --pure $ foldl' (\arr nme -> arr ++ [getFollowers nme]) [] fList
        --fofFList <- 
--unfollowFollowers :: [String] -> String ->
{-unfollowFollowers ufList =
    do
        fObj <- getFollowers "aatishVL"
        fList <- pure (getScrNames fObj)
        filterFollowers <- pure (checkFollowers ufList fList)
        unfollowed <- _todo
        putStrLn unfollowed ++ "\nUnable to Unfollow "++(snd filterFollowers) -}
{-unfollowUsers usrIds =
    let
        urlRes = resUnfollow ++ "?user_id=" ++ (show (head usrIds))
    in postWith authenticator (resunfollow++"?user_id=52544275") (Follower{id = 52544275, name = "Ivanka Trump", screen_name = "IvankaTrump", followers_count= 5526295})
-}

getUserList :: IO([String])
getUserList =
    do
        putStrLn "Enter a delimiter char:"
        delimiter <-getLine
        putStrLn "Enter User ScreenName list seperated by delimiter:"
        userList<-getLine
        pure $ words (map (\c-> if c == (head delimiter) then ' ' else c) userList)

{-unfollowUserList =
    do
    followers <- getFollowers "aatishVL"
    usrs <- getUserList
    validUsers <- checkFollowers usrs (getScrNames followers)
    UnfollowId = 
    pure $ DL.map (\u -> postWith authenticator (resunfollow++"?user_id="++u) (DB.pack "sdsd")) userSet)
-}

unfollowUserList :: IO ()
unfollowUserList =
    let
        followers = getFollowers "aatishVL"
        usrs = getUserList
        validUsers = followers >>= \f -> usrs >>= \us -> pure $ checkFollowers us (getScrNames f)
        unfollowIds = followers >>= \f -> validUsers >>= \us -> pure (getUnfollowerIds f (fst us))
    in unfollowIds >>= \usrSets -> 
        CM.mapM (\u -> postWith authenticator (resunfollow++"?user_id="++(show u)) (DB.pack "SAMPLE TEXT")) usrSets
        >> (validUsers >>= \vu -> putStrLn ("\nUnable to Unfollow : "++(snd vu)))