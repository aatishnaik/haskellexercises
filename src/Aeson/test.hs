{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE OverloadedStrings #-}
module TestTweet where
import GHC.Generics
import Data.Aeson
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

data Person = Person {
      name :: String
    , age  :: Int
    } deriving (Generic, Show)
    
instance ToJSON Person where
    toEncoding = genericToEncoding defaultOptions
    

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "name"
        <*> v .: "age"

--getURLContent:: Int -> IO (Response BL.ByteString)
getURLContent uid =
    let 
        --data Auth is not exposed so have to use oauth1Auth
        urlString = "https://api.twitter.com/1.1/friendships/destroy.json?user_id="++(show uid)
        authKey = oauth1Auth (DB.pack "FaPdykltzoxz65Tf9TH9ugIe4") (DB.pack "4yxJAXPQMQinymZI6CDgWNhMdLAT68zjucqvlpNeS7glDvSEtc") (DB.pack "976334890915397632-DpsHhYJiTKkQY7ip48JFcqK86xbrsBP") (DB.pack "a6C63vZ8sQmFFgQjm1KnRy4VYPKwRy1dfNcFb5nxliYMH")
        opt = defaults & header "Accept" .~ ["application/json"]
        newHead = opt & auth .~ (Just authKey)
    in postWith newHead urlString