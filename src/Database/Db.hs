{-# LANGUAGE OverloadedStrings #-}
module Database.DB where
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import GHC.Int
import Prelude hiding (id)
data Customer = Customer {
    usrid :: Integer,
    uemail :: String,
    uname :: String
}deriving (Show)

instance FromRow Customer where
    fromRow = do
        id <- field
        email <- field
        full_name <- field
        pure $ Customer{usrid=id,uemail=email,uname=full_name}

dbConnection :: IO Connection
dbConnection = connect ConnectInfo{connectHost ="localhost",connectPort = 5432,connectUser ="b2b",connectPassword ="b2b",connectDatabase ="b2b"}

displayCustomer :: IO [Customer]
displayCustomer = do
    conn <- dbConnection
    query_ conn "SELECT id,email,full_name FROM customers" :: IO [Customer]

insertCustomer :: IO GHC.Int.Int64
insertCustomer = do
    conn <- dbConnection
    execute_ conn "INSERT INTO customers (customer_ref,title,full_name,email,phone,client_id,number_of_bookings,created_at,updated_at) VALUES ('ablabla','test','name2','aat2@aat.com','8675848382',3,3,'2018-04-09 17:21:49.921882','2018-04-09 17:21:49.921882')"

updateCustomer :: IO GHC.Int.Int64
updateCustomer = do
    conn <- dbConnection
    execute_ conn "UPDATE customers SET customer_ref='bbllaa',title='title1334',full_name='updated',email='up@up.com',phone='7341134343',client_id=4,number_of_bookings=1,created_at='2018-04-09 17:21:49.921882',updated_at='2018-04-09 17:21:49.921882' WHERE id = 2"

deleteCustomer :: IO GHC.Int.Int64
deleteCustomer = do
    conn <- dbConnection
    execute_ conn "DELETE FROM customers WHERE id = 2"