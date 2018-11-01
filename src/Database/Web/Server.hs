{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Page where
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
    custEmail :: String,
    custName :: String
}deriving (Show)

instance FromRow Customer where
    fromRow = do
        id <- field
        email <- field
        full_name <- field
        pure $ Customer{custId=id,custEmail=email,custName=full_name}

type API = 
    "index" :> PageAPI
    :<|> "index" :> ViewAPI
    :<|> "index" :> UpdateAPI
    :<|> "index" :> InsertAPI
    :<|> "index" :> DeleteAPI
type PageAPI =  Get '[HTML] (Html())
type ViewAPI = "view_customers" :> Get '[HTML] (Html())
type UpdateAPI = "update_one_customer" :> Get '[HTML] (Html())
type InsertAPI = "insert_customer" :> Get '[HTML] (Html())
type DeleteAPI = "delete_customer" :> Get '[HTML] (Html())
server :: Server API
server = pageServer :<|> viewServer :<|> updateServer :<|> insertServer :<|> deleteServer
pageServer :: Server PageAPI
pageServer = return page
viewServer :: Server ViewAPI
viewServer =  return customerPg
updateServer :: Server UpdateAPI
updateServer =  return updateOnePg
insertServer :: Server InsertAPI
insertServer =  return insertPg
deleteServer :: Server DeleteAPI
deleteServer =  return deletePg

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app

page :: Html()
page = 
      html_
        (do 
          head_
              (do 
                title_ "Home"
                link_ [rel_ "stylesheet",type_ "text/css",href_ ""]
                  )
          body_
              (do 
                br_ []
                a_ [href_ "http://localhost:8081/index/view_customers"] "View Customers "
                br_ []
                a_ [href_ "http://localhost:8081/index/update_one_customer"] "Edit Customers "
                br_ []
                a_ [href_ "http://localhost:8081/index/delete_customer"] "Delete Customers "
                br_ []
                a_ [href_ "http://localhost:8081/index/insert_customer"] "Add Customers "
                  ))

customerPg :: Html()
customerPg = 
      html_
        (do 
          head_
              (do 
                title_ "Customers"
                link_ [rel_ "stylesheet",type_ "text/css",href_ ""]
                  )
          body_
              (do 
                br_ []
                p_ "View Customers "
                br_ []
                --(displayCustomerPage)
                  ))

updateOnePg  :: Html()
updateOnePg = 
    html_
      (do head_
              (do title_ "Update Customer"
                  link_ [rel_ "stylesheet",type_ "text/css",href_ ""]
                  )
          body_
              (do div_ [id_ "header",style_ "color:red"] "Form Customer"
                  form_ [id_ "insertCustomer"]
                      (do 
                          br_ []
                          label_ "Choose  customer: "
                          --select_
                          --do (mapM_ (\c ->
                              --let (v1,v2)=customerToHtml c
                              --in li_ v1) list)
                          br_ []
                          label_ "Enter Value "
                          input_ [type_ "text",name_ "value"]
                          br_ []
                          input_ [ name_ "submit",type_ "submit",value_ "Submit"]
                              )))

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

deletePg :: Html()
deletePg = 
    html_
      (do head_
              (do title_ "Delete Customer"
                  link_ [rel_ "stylesheet",type_ "text/css",href_ ""]
                  )
          body_
              (do div_ [id_ "header",style_ "color:red"] "Form Customer"
                  form_ [id_ "insertCustomer"]
                      (do 
                          br_ []
                          label_ "Choose  customer: "
                          {-select_
                          do 
                            (mapM_ (\c ->
                              let (v1)=customerToHtml c
                              in li_ v1) displayCustomerList)-}
                          input_ [ name_ "submit",type_ "submit",value_ "Submit"]
                              )))
  
getCustomers :: ReaderT Connection IO[Customer]
getCustomers = do
    conn <- ask
    list <- liftIO $query_ conn "SELECT id,full_name,email FROM customers" :: ReaderT Connection IO[Customer]
    liftIO $ pure list

customerPage :: [Customer] -> Html()
customerPage list =
    html_
        (do head_
                (do title_ "CustomerList"
                    link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
                    )
            body_
                (do div_ [id_ "header",style_ "color:white"] "Table Customer"
                    table_ 
                        (do 
                            tr_ 
                                (do th_ "name"
                                    th_ "email")
                            mapM_ (\c ->
                                let (v1,v2)=customerToHtml c
                                in tr_ $ do td_ v1 
                                            td_ v2) list)))
dbConnection :: IO Connection
dbConnection = connect ConnectInfo{connectHost ="localhost",connectPort = 5432,connectUser ="b2b",connectPassword ="b2b",connectDatabase ="b2b"}

customerToHtml :: Customer->(Html(),Html())
customerToHtml customer = 
    let Customer{custId=_,custEmail=e,custName=n}=customer
    in (toHtml e,toHtml n)

{-displayCustomerPage :: IO (Html())
displayCustomerPage = do
    conn <- dbConnection
    fmap (\c->customerPage c) (runReaderT getCustomers conn)-}
    
