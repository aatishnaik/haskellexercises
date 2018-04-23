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

data Customer = Customer
    { custid :: Int,
      custname :: String,
      custemail :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Customer

customers  :: [Customer]
customers  =
  [ Customer {custid=13,custname="abcn",custemail="abc@abc.com"}
  , Customer {custid=34,custname="bbc",custemail="bbc@bbc.com"}
  ]

type CustomerAPI = "customers" :> Get '[JSON] [Customer]

server :: Server CustomerAPI
server = return customers

customerAPI :: Proxy CustomerAPI
customerAPI = Proxy

main :: IO ()
main = run 8081 (serve customerAPI server)