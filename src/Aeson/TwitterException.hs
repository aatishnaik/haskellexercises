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
import Data.List.Split as DLS
import Data.String.Conv as DSC

resGetFollowers :: String
resGetFollowers = "https://api.twitter.com/1.1/friends/list.json"

resunfollow :: String
resunfollow = "https://api.twitter.com/1.1/friendships/destroy.json"

resfollow :: String
resfollow = "https://api.twitter.com/1.1/friendships/create.json"

resverify :: String
resverify = "https://api.twitter.com/1.1/users/show.json"

type UserId = Integer
type UserName = String
type UserScreenName = String
type UserFollowerCount = Integer

data User = User {
    id :: UserId,
    name :: UserName ,
    screenName :: UserScreenName,
    followersCount :: UserFollowerCount
}deriving (Show,Eq,Generic)

instance FromJSON User where
    parseJSON =  
        withObject "User" $ \v ->  do
            uid <- v .: "id"
            uname <- v .: "name"
            uscreenName <- v .: "screen_name"
            ufollowersCount <- v .: "followers_count"
            pure User {
                id=uid,
                name=uname,
                screenName=uscreenName,
                followersCount=ufollowersCount
            }

data UserList = UserList {
    userList :: [User]
}deriving (Show,Eq,Generic)

instance FromJSON UserList where
    parseJSON = 
        withObject "User" $ \v ->  do
            userlst <- v .: "users"
            pure UserList {userList = userlst}

authenticator :: Options
authenticator =
    let 
        authKey = oauth1Auth ((DSC.toS ("FaPdykltzoxz65Tf9TH9ugIe4"::String))::DB.ByteString) ((DSC.toS ("4yxJAXPQMQinymZI6CDgWNhMdLAT68zjucqvlpNeS7glDvSEtc"::String))::DB.ByteString) ((DSC.toS ("976334890915397632-DpsHhYJiTKkQY7ip48JFcqK86xbrsBP"::String))::DB.ByteString) ((DSC.toS ("a6C63vZ8sQmFFgQjm1KnRy4VYPKwRy1dfNcFb5nxliYMH"::String))::DB.ByteString)
        --def = set NW.checkResponse (Just $ \_ _ -> return ()) defaults
        --opt = def & NW.auth ?~ authKey
        opt = defaults & NW.auth ?~ authKey
    in opt

getFollowers :: String -> IO (Either String UserList)
getFollowers userName =
    let urlStr = resGetFollowers++"?cursor=-1&screen_name="++userName++"&skip_status=true&include_user_entities=false"
        userData = getWith authenticator urlStr
    in do
        udata <- userData
        pure (eitherDecode (udata ^. responseBody))

--extracts screen name from list of follower objects
getScrNames :: (Either String UserList) -> [String]
getScrNames fList = 
    let Right UserList {userList = follArr} = fList
    in DL.map (\User{
        id=_,name=_,screenName=scrname,followersCount=_
    } -> scrname) follArr

splitFollowers :: [String] -> [String] -> ([String],[String])
splitFollowers ufList fList = 
    let
        fol = ufList `intersect` fList
        ufol = DL.foldl' (\arr f-> if(f `notElem` fList) then arr++[f] else arr) [] ufList
    in (fol,ufol)

getUnfollowerIds :: (Either String UserList) -> [String] -> [Integer]
getUnfollowerIds fList ufList= 
    let Right UserList {userList = follArr} = fList
    in DL.foldl' (\arr User{
        id=usrid,name=_,screenName=scrName,followersCount=_
    } -> if (scrName `elem` ufList) then arr++[usrid] else arr) [] follArr

getUserList :: IO[String]
getUserList =
    do
        putStrLn "Enter a delimiter char:"
        delimiter <- getLine
        putStrLn "Enter User ScreenName list seperated by delimiter:"
        usersList <- getLine
        pure $ splitOn delimiter usersList

unfollowUserList :: IO ()
unfollowUserList =
    do
        f <- getFollowers "aatishVL"
        usrs <- getUserList
        (validUsers,invalidUsers) <- pure $ splitFollowers usrs (getScrNames f)
        unfollowIds <- pure (getUnfollowerIds f validUsers)
        ufStr <- pure ("\nSuccessfully Unfollowed :"++
            (foldl' (\arr u-> arr++" "++u) "" validUsers)++"\nUnable to Unfollow :"++(concat invalidUsers))
        CM.mapM (\u -> (postWith authenticator (resunfollow++"?user_id="++(show u)) (DB.pack "SAMPLE TEXT"))) unfollowIds >> putStrLn ufStr

followUserList :: IO ()
followUserList =
    do
        f <- getFollowers "aatishVL"
        usrs <- getUserList
        (iUsers,vUsers) <- pure $ splitFollowers usrs (getScrNames f)
        validUsers <- CM.foldM (\arr u -> checkScrName u >>= \sc ->
            case sc of
                Right val -> if (val ^. responseStatus ^. statusCode)==200
                    then pure (arr++[u])
                    else pure arr
                Left _ -> pure arr
            ) [] vUsers
        CM.mapM (\u -> (postWith authenticator (resfollow++"?screen_name="++u++"&follow=true") (DB.pack "SAMPLE TEXT"))) validUsers 
            >> putStrLn ("Are already being followed:"++(foldl' (\arr u-> arr++" "++u) "" iUsers)
            ++"\nNow following:"++(foldl' (\arr u-> arr++" "++u) "" validUsers))

checkScrName :: String -> IO (Either SomeException (Response BL.ByteString))--IO (Int)
checkScrName userName =
    let urlStr = resverify++"?screen_name="++userName
        userData = try (getWith authenticator urlStr)
    in userData

twitterBot :: IO()
twitterBot =
    do
        putStrLn "Enter 1 for mass unfollow:\nEnter 2 for mass follow:"
        ch <- getLine
        case ((readMaybe ch)::Maybe Int) of
            Just 1 -> unfollowUserList
            Just 2 -> followUserList
            _ -> putStrLn "Invalid choice" >> twitterBot
        