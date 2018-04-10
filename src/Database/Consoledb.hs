{-# LANGUAGE OverloadedStrings #-}
module Database.Consoledb where
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.List
import Text.Read
import GHC.Int
import Prelude hiding (id)
data Customer = Customer {
    cid :: Integer,
    cemail :: String,
    cname :: String
}deriving (Show)

instance FromRow Customer where
    fromRow = do
        id <- field
        email <- field
        full_name <- field
        pure Customer{cid=id,cemail=email,cname=full_name}

dbConnection :: IO Connection
dbConnection = connect ConnectInfo{connectHost ="localhost",connectPort = 5432,connectUser ="b2b",connectPassword ="b2b",connectDatabase ="b2b"}

displayCustomer :: IO ()
displayCustomer = do
    conn <- dbConnection
    list <- query_ conn "SELECT id,email,full_name FROM customers" :: IO [Customer]
    opStr <- pure (foldl' (\arr Customer{cid=_,cemail=e,cname=n} -> arr++n++" "++e++"\n") "Customer List:\n" list)
    putStrLn opStr

insertCustomer :: IO ()
insertCustomer = do
    conn <- dbConnection
    res <- execute_ conn "INSERT INTO customers (customer_ref,title,full_name,email,phone,client_id,number_of_bookings,created_at,updated_at) VALUES ('ablabla','test','name2','aat2@aat.com','8675848382',3,3,'2018-04-09 17:21:49.921882','2018-04-09 17:21:49.921882')"
    if res > 0 then
        putStrLn "Customer Inserted"
    else
        putStrLn "Insertion failed"

updateCustomer :: IO ()
updateCustomer = do
    conn <- dbConnection
    res <- execute_ conn "UPDATE customers SET customer_ref='bbllaa',title='title1334',full_name='updated',email='up@up.com',phone='7341134343',client_id=4,number_of_bookings=1,created_at='2018-04-09 17:21:49.921882',updated_at='2018-04-09 17:21:49.921882' WHERE id = 2"
    if res > 0 then
        putStrLn "Customer Updated"
    else
        putStrLn "Update failed"

deleteCustomer :: IO ()
deleteCustomer = do
    conn <- dbConnection
    res <- execute_ conn "DELETE FROM customers WHERE id = 2"
    if res > 0 then
        putStrLn "Customer Deleted"
    else
        putStrLn "Delete failed"

consoleDb :: IO()
consoleDb = do
    putStrLn "Enter 1 to Insert\nEnter 2 to Update\nEnter 3 to Delete\nEnter 4 to View\nEnter 0 to Exit\n"
    ch <- getLine
    case ((readMaybe ch)::Maybe Int) of
        Just 1 -> insertCustomer
        Just 2 -> updateCustomer
        Just 3 -> deleteCustomer
        Just 4 -> displayCustomer
        Just 0 -> putStrLn "Exiting..."
        _ -> putStrLn "Invalid choice" >> consoleDb