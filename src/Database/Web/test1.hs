{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude 
import Data.Aeson.Types
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

data User = User
    { name :: String
    , age :: Int
    , email :: String
    } deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users =
  [ User {name="abcn",age=13,email="abc@abc.com"}
  , User {name="bbc",age=34,email="bbc@bbc.com"}
  ]

type UserAPI = "users" :> Get '[JSON] [User]

server :: Server UserAPI
server = return users

userAPI :: Proxy UserAPI
userAPI = Proxy

main :: IO ()
main = run 8081 (serve userAPI server)