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
import Lucid
data Customer = Customer {
    custId :: Integer,
    custEmail :: String,
    custName :: String
}deriving (Eq, Show, Generic)

type UserAPI = "customers" :> Get '[JSON] [Customer]

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = return customerU

main :: IO ()
main = run 8081 (serve userAPI server)

customerU :: Html()
customerU =
    html_
    (do head_
            (do title_ "New Customer"
                link_ [rel_ "stylesheet",type_ "text/css",href_ ""]
                )
        body_
            (do div_ [id_ "header",style_ "color:red"] "Form Customer"
                form_ [id_ "insertCustomer"]
                    (do 
                        br_ []
                        label_ "Enter Value "
                        input_ [type_ "text",name_ "value"]
                        br_ []
                        input_ [ name_ "submit",type_ "submit",value_ "Submit"]
                            )))