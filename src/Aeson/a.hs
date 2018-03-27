{-# LANGUAGE OverloadedStrings #-}
module Test3wreq where

import Network.Wreq
import Control.Lens as CL
import Data.Aeson as DA 
import Data.ByteString.Char8 as DB
import Prelude hiding (id)

data User = User {
    ids :: [Int]
    } deriving Show

instance FromJSON User where
    parseJSON = withObject "User" $ \v -> do
        id1 <- v .: "ids"
        return $ User { ids = id1}

data Responses = Responses {
    users :: User,
    next_cursor :: Int,
    next_cursor_str :: String,
    previous_cursor :: Int,
    previous_cursor_str :: String

} deriving Show

instance FromJSON Responses where
    parseJSON = withObject "Responses" $ \v -> do
        --users1 <- v .: "users"
        users1 <- parseJSON (Object v)
        next_cursor1 <- v .: "next_cursor"
        next_cursor_str1 <- v .: "next_cursor_str"
        previous_cursor1 <- v .: "previous_cursor"
        previous_cursor_str1 <- v .: "previous_cursor_str"
        return $ Responses {users=users1, next_cursor=next_cursor1, next_cursor_str=next_cursor_str1, previous_cursor=previous_cursor1, previous_cursor_str=previous_cursor_str1}

test =do 
        let auth1 = oauth1Auth (DB.pack "mqC2gEAkGVxVjZJ9Drn5k08A7") (DB.pack  "oj6gKUWjFwLVo9UVUzvayFvfmf4vVbpu7RV0K4ZUTXAoz8v5GY") (DB.pack "976346669745778688-BIIdmsBNd4BUKkLG3LlCwt1AfOHwWyH") (DB.pack "1Btf70miGr2eg7JsAKmji1QLECDeIhwWNvjwGu869YmX3")
            opts = defaults & auth ?~ auth1
         in 
            do
                r <- getWith opts "https://api.twitter.com/1.1/followers/ids.json?screen_name=StanleyFloFer%20"
                return $ r^. responseBody 

test123 =
    DA.decode "{\"ids\":[976346669745778688,103216277,194973597,36971046,2237391301,533133778,1226755262,564531009,474675321,3035582575,2278645093,3004065854,603242882,2865334267,2809230536,1326956275,1893487717,840055555,282486592,2314224804,82796097,1883687545,2308104366,2151422309,2314331928,589709851,153726753,26458169,418530768,899609510,1285352616,1380311581,1364105731,1190849526,471891513,241508907,1050175231,175987026,222362698],\"next_cursor\":0,\"next_cursor_str\":\"0\",\"previous_cursor\":0,\"previous_cursor_str\":\"0\"}" :: Maybe Responses
	
