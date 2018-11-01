{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module TestTweet where
import Control.Lens
import Data.Aeson as DA
import Network.Wreq
import GHC.Generics
import qualified Data.ByteString.Char8 as DB
import qualified Data.ByteString.Lazy as BL
import Data.Map as Map
import Prelude hiding (id)
resUrl :: String
resUrl = "https://api.twitter.com/1.1/followers/list.json?cursor=-1&screen_name=aatishVL&skip_status=true&include_user_entities=false"

data User = User {
    id :: Integer,
    id_str :: String,
    name :: String ,
    screen_name :: String,
    location :: String,
    description :: String,
    protected :: Bool,
    followers_count :: Int,
    friends_count :: Int,
    listed_count :: Int,
    created_at :: String,
    favourites_count :: Int,
    geo_enabled :: Bool,
    verified :: Bool,
    statuses_count :: Int,
    lang :: String,
    contributors_enabled :: Bool,
    is_translator :: Bool,
    is_translation_enabled :: Bool,
    following :: Bool,
    follow_request_sent :: Bool,
    notifications :: Bool
}deriving (Show,Eq,Generic)

instance FromJSON User where
    parseJSON (Object v) = 
        do
        uid <- v .: "id"
        uid_str <- v .: "id_str"
        uname <- v .: "name"
        uscreen_name <- v .: "screen_name"
        ulocation <- v .: "location"
        udescription <- v .: "description"
        uprotected <- v .: "protected"
        ufollowers_count <- v .: "followers_count"
        ufriends_count <- v .: "friends_count"
        ulisted_count <- v .: "listed_count"
        ucreated_at <- v .: "created_at"
        ufavourites_count <- v .: "favourites_count"
        ugeo_enabled <- v .: "geo_enabled"
        uverified <- v .: "verified"
        ustatuses_count <- v .: "statuses_count"
        ulang <- v .: "lang"
        ucontributors_enabled <- v .: "contributors_enabled"
        uis_translator <- v .: "is_translator"
        uis_translation_enabled <- v .: "is_translation_enabled"
        ufollowing <- v .: "following"
        ufollow_request_sent <- v .: "follow_request_sent"
        unotifications <- v .: "notifications"
        pure User {
            id=uid,
            id_str=uid_str,
            name=uname,
            screen_name=uscreen_name,
            location=ulocation,
            description=udescription,
            protected=uprotected,
            followers_count=ufollowers_count,
            friends_count=ufriends_count,
            listed_count=ulisted_count,
            created_at=ucreated_at,
            favourites_count=ufavourites_count,
            geo_enabled=ugeo_enabled,
            verified=uverified,
            statuses_count=ustatuses_count,
            lang=ulang,
            contributors_enabled=ucontributors_enabled,
            is_translator=uis_translator,
            is_translation_enabled=uis_translation_enabled,
            following=ufollowing,
            follow_request_sent=ufollow_request_sent,
            notifications=unotifications
        }
    parseJSON _ = pure User {}

data UserList = UserList {
    userList :: [User],
    next_cursor :: Integer,
    next_cursor_str::String,
    previous_cursor::Integer,
    previous_cursor_str::String
}deriving (Show,Eq,Generic)

instance FromJSON UserList where
    parseJSON (Object v) = 
        do
            userlst <- v .: "users"
            nxtcur <- v .: "next_cursor"
            snxtcur <- v .: "next_cursor_str"
            precur <- v .: "previous_cursor"
            sprecur <- v .: "previous_cursor_str"
            pure (UserList {userList = userlst,next_cursor=nxtcur,next_cursor_str=snxtcur,previous_cursor=precur,previous_cursor_str=sprecur})

    parseJSON _ = pure UserList{}

getDecode :: (Response BL.ByteString) -> IO (Either String UserList)
getDecode str = pure (DA.eitherDecode (str ^. responseBody))

type Resp = Response (Map String Value)

getURLContent :: String -> IO (Map String Value)
getURLContent urlString =
    let 
        authKey = oauth1Auth (DB.pack "FaPdykltzoxz65Tf9TH9ugIe4") (DB.pack "4yxJAXPQMQinymZI6CDgWNhMdLAT68zjucqvlpNeS7glDvSEtc") (DB.pack "976334890915397632-DpsHhYJiTKkQY7ip48JFcqK86xbrsBP") (DB.pack "a6C63vZ8sQmFFgQjm1KnRy4VYPKwRy1dfNcFb5nxliYMH")
        opt = defaults & auth ?~ authKey
    in do
        r <- asJSON =<< getWith opt urlString ::IO Resp
        pure $ r ^. responseBody

--by changing the header
getURLContent2:: String -> IO (Either String UserList)
getURLContent2 urlString =
    let 
        --data Auth is not exposed so have to use oauth1Auth
        authKey = oauth1Auth (DB.pack "FaPdykltzoxz65Tf9TH9ugIe4") (DB.pack "4yxJAXPQMQinymZI6CDgWNhMdLAT68zjucqvlpNeS7glDvSEtc") (DB.pack "976334890915397632-DpsHhYJiTKkQY7ip48JFcqK86xbrsBP") (DB.pack "a6C63vZ8sQmFFgQjm1KnRy4VYPKwRy1dfNcFb5nxliYMH")
        opt = defaults & header "Accept" .~ ["application/json"]
        newHead = opt & auth .~ (Just authKey)
        dec = getWith newHead urlString
    in dec >>= \content -> getDecode (content)