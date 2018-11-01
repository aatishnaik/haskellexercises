{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Test where
import Lucid.Base
import Servant.Client
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Lucid
import Servant.HTML.Lucid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Monad.Reader
import Prelude hiding (id)

data Customer = Customer {
    custId :: Integer,
    custReff :: String,
    custTitle :: String,
    custFullName:: String,
    custEmail :: String,
    custClientId :: Integer,
    custNumberOfBookings :: Integer
}deriving (Show)

instance FromRow Customer where
    fromRow = do
        id <- field
        customer_ref <- field
        title <- field
        full_name <- field
        email <- field
        client_id <- field
        number_of_bookings <- field
        pure $ Customer{custId=id,custReff=customer_ref,custTitle=title,custFullName=full_name,custEmail=email,custClientId =client_id,custNumberOfBookings=number_of_bookings}

dbConnection :: IO Connection
dbConnection = connect ConnectInfo{connectHost ="localhost",connectPort = 5432,connectUser ="b2b",connectPassword ="b2b",connectDatabase ="b2b"}

{-type InsertAPI = "insertCust" :> Get '[HTML] Html
  :> Capture "custId" Integer
  :> Capture "custCustomerRef" String
  :> Capture "custTitle" String
  :> Capture "custFullName" String
  :> Capture "custEmail" String
  :> Capture "custPhone" String
  :> Capture "custClientId" Integer
  :> Capture "custNumberOfBookings" Integer
  :> Post '[HTML] Customer

customer :: Integer -> String -> String -> String -> String -> Integer -> Integer -> Customer
customer cid cref ctit cfn cemail ccid cnob = Customer {
  custId =cid,
  custReff =cref,
  custTitle =ctit,
  custFullName =cfn,
  custEmail =cemail,
  custClientId =ccid,
  custNumberOfBookings =cnob
}-}

data InsertPg = InsertPg
type InsertAPI = "insert_customer" :> Get '[HTML] InsertPg

insertServer :: Server InsertPg
insertServer =  return InsertAPI

customerForm :: Html()
customerForm  =
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