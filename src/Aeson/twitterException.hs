{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Aeson.TwitterException where
import Data.List as DL
import Control.Monad as CM
import Control.Lens
import Data.Aeson as DA
import Network.Wreq as NW
import Text.Read as TR
import GHC.Generics
import qualified Data.ByteString.Char8 as DB
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (id)
import Control.Exception

resGetFollowers :: String
resGetFollowers = "https://api.twitter.com/1.1/friends/list.json"

resunfollow :: String
resunfollow = "https://api.twitter.com/1.1/friendships/destroy.json"

resfollow :: String
resfollow = "https://api.twitter.com/1.1/friendships/create.json"

resverify :: String
resverify = "https://api.twitter.com/1.1/users/show.json"

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
    parseJSON _ = error "Invalid Input"

data FollowerList = FollowerList {
    followerList :: [Follower]
}deriving (Show,Eq,Generic)

instance FromJSON FollowerList where
    parseJSON (Object v) = 
        do
            userlst <- v .: "users"
            pure (FollowerList {followerList = userlst})
    parseJSON _ = error "Invalid Input"

authenticator :: Options
authenticator =
    let 
        authKey = oauth1Auth (DB.pack "FaPdykltzoxz65Tf9TH9ugIe4") (DB.pack "4yxJAXPQMQinymZI6CDgWNhMdLAT68zjucqvlpNeS7glDvSEtc") (DB.pack "976334890915397632-DpsHhYJiTKkQY7ip48JFcqK86xbrsBP") (DB.pack "a6C63vZ8sQmFFgQjm1KnRy4VYPKwRy1dfNcFb5nxliYMH")
        --def = set NW.checkResponse (Just $ \_ _ -> return ()) defaults
        --opt = def & NW.auth ?~ authKey
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

checkFollowers :: [String] -> [String] -> ([String],String)
checkFollowers ufList fList = 
    let
        fol = ufList `intersect` fList
        ufol = DL.foldl' (\arr f-> if(f `notElem` fList) then arr++" "++f else arr) "" ufList
    in (fol,ufol)

getUnfollowerIds :: (Either String FollowerList) -> [String] -> [Integer]
getUnfollowerIds fList ufList= 
    let Right FollowerList {followerList = follArr} = fList
    in DL.foldl' (\arr Follower{
        id=usrid,name=_,screen_name=scrName,followers_count=_
    } -> if (scrName `elem` ufList) then arr++[usrid] else arr) [] follArr

getUserList :: IO([String])
getUserList =
    do
        putStrLn "Enter a delimiter char:"
        delimiter <-getLine
        putStrLn "Enter User ScreenName list seperated by delimiter:"
        userList <-getLine
        pure $ words (map (\c-> if c == (head delimiter) then ' ' else c) userList)

unfollowUserList :: IO ()
unfollowUserList =
    do
        f <- getFollowers "aatishVL"
        usrs <- getUserList
        (validUsers,invalidUsers) <- pure $ checkFollowers usrs (getScrNames f)
        unfollowIds <- pure (getUnfollowerIds f validUsers)
        ufStr <- pure ("\nSuccessfully Unfollowed :"++
            (foldl' (\arr u-> arr++" "++u) "" validUsers)++"\nUnable to Unfollow :"++invalidUsers)
        CM.mapM (\u -> (postWith authenticator (resunfollow++"?user_id="++(show u)) (DB.pack "SAMPLE TEXT"))) unfollowIds >> putStrLn ufStr

followUserList :: IO ()
followUserList =
    do
        f <- getFollowers "aatishVL"
        usrs <- getUserList
        (iUsers,vUsers) <- pure $ checkFollowers usrs (getScrNames f)
        validUsers <- CM.foldM (\arr u -> checkScrName u >>= \sc ->
            case sc of
                Right val -> if (val ^. responseStatus ^. statusCode)==200
                    then pure (arr++[u])
                    else pure arr
                Left _ -> pure arr
            ) [] (words vUsers)
        CM.mapM (\u -> (postWith authenticator (resfollow++"?screen_name="++u++"&follow=true") (DB.pack "SAMPLE TEXT"))) validUsers 
            >> putStrLn ("Are already being followed:"++(foldl' (\arr u-> arr++" "++u) "" iUsers)
            ++"\nNow following:"++(foldl' (\arr u-> arr++" "++u) "" validUsers))

checkScrName :: String -> IO (Either SomeException (Response BL.ByteString))--IO (Int)
checkScrName userName =
    let urlStr = resverify++"?screen_name="++userName
        userData = try (getWith authenticator urlStr) -- :: IO (Either SomeException (Response BL.ByteString))
    in userData {->>= \udata -> 
        case udata of
            Right val -> pure (val ^. responseStatus ^. statusCode)
            Left _ -> pure 403-}

twitterBot :: IO()
twitterBot =
    do
        putStrLn "Enter 1 for mass unfollow:\nEnter 2 for mass follow:"
        ch <- getLine
        case ((readMaybe ch)::Maybe Int) of
            Just 1 -> unfollowUserList
            Just 2 -> followUserList
            _ -> putStrLn "Invalid choice" >> twitterBot
        