{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module T3 where
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Lucid
import Servant.HTML.Lucid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Monad.Reader
import Prelude hiding (id)
import GHC.Generics
data Customer = Customer {
    custId :: Integer,
    custEmail :: String,
    custName :: String
}deriving (Show,Eq)

type API = 
    "index" :> PageAPI
    :<|> "check" :> ViewAPI

type PageAPI =   Get '[HTML] (Html())
    :<|>"check"
        :> ReqBody '[FormUrlEncoded] Customer
        :> Post '[HTML] Html
type ViewAPI = "view_customers" :> Get '[HTML] (Html())

server :: Server API
server = pgServer
pgServer :: Server PageAPI
pgServer = return insertPg
api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app

insertPg :: Html()
insertPg = 
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
                        label_ "Customer Reference: "
                        input_ [type_ "text",name_ "customerreff"]
                        br_ []
                        label_ "Title: "
                        input_ [type_ "text",name_ "title"]
                        br_ []
                        label_ "FullName: "
                        input_ [type_ "text",name_ "fullname"]
                        br_ [] 
                        label_ "Email: "
                        input_ [type_ "text",name_ "email"]
                        br_ []
                        label_ "Phone: "
                        input_ [type_ "text",name_ "phone"]
                        br_ []
                        label_ "Client Id: "
                        input_ [type_ "text",name_ "clientid"]
                        br_ []
                        label_ "Number of Bookings: "
                        input_ [type_ "number",name_ "numberofbookings"]
                        br_ [] 
                        input_ [ name_ "submit",type_ "submit",value_ "Submit"]
                            )))
