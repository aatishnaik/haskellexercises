{-# LANGUAGE OverloadedStrings #-}
module Database.NewConsoledb where
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Control.Monad as CM
import Control.Monad.Reader
import Data.Time.LocalTime
import Data.List
import Text.Read
import GHC.Int
import Prelude hiding (id)
newtype CustomerId = CustomerId (Maybe Integer) deriving (Show)
data Customer = Customer {
    custId :: CustomerId,
    custEmail :: String,
    custName :: String,
    custReff :: String,
    custTitle :: String,
    custPhone :: String,
    custClId :: Integer,
    custnBook :: Integer,
    custCreatedAt :: LocalTime,
    custUpdatedAt :: LocalTime
}deriving (Show)

instance FromRow Customer where
    fromRow = do
        id <- field
        email <- field
        full_name <- field
        customer_ref <- field
        title <- field
        phone <- field
        client_id <- field
        number_of_bookings <- field
        created_at <- field
        updated_at <- field
        pure Customer{custId=CustomerId (Just id),custEmail=email,custName=full_name,custReff=customer_ref,custTitle=title,custPhone=phone,custClId=client_id,custnBook=number_of_bookings,custCreatedAt=created_at,custUpdatedAt=updated_at}

instance ToRow Customer where
    toRow r = [toField (custEmail r),toField (custName r),toField (custReff r),toField (custTitle r),toField (custPhone r),toField (custClId r),toField (custnBook r),toField (custCreatedAt r),toField (custUpdatedAt r)]

dbConnection :: IO Connection
dbConnection = connect ConnectInfo{connectHost ="localhost",connectPort = 5432,connectUser ="b2b",connectPassword ="b2b",connectDatabase ="b2b"}

displayCustomers :: ReaderT Connection IO()
displayCustomers = do
    conn <- ask
    --query_ conn "SELECT full_name,email FROM customers" :: IO [(String,String)]
    list <- liftIO $ query_ conn "SELECT full_name,email FROM customers" :: ReaderT Connection IO [(String,String)]
    opStr <- pure $ intercalate "\n" (map (\(fn,em) -> fn++" "++em) list)
    liftIO $ putStrLn opStr

setCustomer :: IO Customer
setCustomer = do
    putStr "Enter Customer_ref: "
    cref <- getLine
    putStr "Enter Title: "
    tl <- getLine
    putStr "Enter Fullname: "
    fn <- getLine
    putStr "Enter Email: "
    em <- getLine
    putStr "Enter Phone: "
    ph <- getLine
    putStr "Enter ClientId: "
    cliid <- getLine
    putStr "Enter No of Bookings: "
    noob <- getLine
    case (readMaybe cliid :: Maybe Integer,readMaybe noob :: Maybe Integer)of
        (Just cid,Just nob) -> pure Customer{custId=CustomerId Nothing,custEmail=em,custName=fn,custReff=cref,custTitle=tl,custPhone=ph,custClId=cid,custnBook=nob,custCreatedAt=(read "2018-04-09 17:21:49.921882" :: LocalTime),custUpdatedAt=(read "2018-04-09 17:21:49.921882" :: LocalTime)}
        _ -> error "Invalid Data"
        
setCustId :: Integer -> Customer -> IO Customer
setCustId i Customer{custId=_,custEmail=em,custName=fn,custReff=cref,custTitle=tl,custPhone=ph,custClId=cid,custnBook=nob,custCreatedAt=ca,custUpdatedAt=ua} = pure Customer{custId=CustomerId (Just i),custEmail=em,custName=fn,custReff=cref,custTitle=tl,custPhone=ph,custClId=cid,custnBook=nob,custCreatedAt=ca,custUpdatedAt=ua}

insertCustomer :: ReaderT Connection IO()
insertCustomer = do
    conn <- ask
    customer <- liftIO $ setCustomer
    res <- liftIO $ execute conn "INSERT INTO customers (customer_ref,title,full_name,email,phone,client_id,number_of_bookings,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?)" customer
    if res > 0 then
        liftIO $ putStrLn "Customer Inserted"
    else
        liftIO $ putStrLn "Insertion failed"

updateCustomer :: ReaderT Connection IO()
updateCustomer = do
    liftIO $ putStrLn "Enter 1 to update single value\nEnter 2 to update entire customer\nEnter 0 to cancel update\n"
    ch <- liftIO $ getLine
    liftIO $ putStr "Enter Id of customer to update: "
    i <- liftIO $ getLine
    case ((readMaybe ch)::Maybe Int) of
        Just 1 -> customerUpdateOne ((readMaybe i):: Maybe Integer)
        Just 2 -> customerUpdateAll ((readMaybe i)::Maybe Integer)
        Just 0 -> liftIO $ putStrLn "Update cancelled.."
        _ -> liftIO $ putStrLn "Invalid choice"

customerUpdateOne :: Maybe Integer -> ReaderT Connection IO()
customerUpdateOne nid = do
    conn <- ask
    liftIO $ putStrLn "Enter 1 to update Customer Ref\nEnter 2 to update Title\nEnter 3 to update Fullname\nEnter 4 to update Email\nEnter 5 to update Phone\nEnter 6 to update Client Id\nEnter 7 to update Bookings\nEnter 0 to Cancel\n"
    ch <- liftIO $ getLine
    liftIO $ putStr "Enter New Value: "
    val <- liftIO $ getLine
    res <- case nid of
        Just newid -> case (readMaybe ch :: Maybe Int) of
            Just 1 ->liftIO $ execute conn "UPDATE customers SET cref=upd.customer_ref FROM (VALUES (?,?)) as upd(uid,cref) WHERE id = upd.uid" (newid:: Integer,val)
            Just 2 ->liftIO $ execute conn "UPDATE customers SET title=upd.utitle FROM (VALUES (?,?)) as upd(uid,utitle) WHERE id = upd.uid" (newid:: Integer,val)
            Just 3 ->liftIO $ execute conn "UPDATE customers SET full_name=upd.ufname FROM (VALUES (?,?)) as upd(uid,ufname) WHERE id = upd.uid" (newid:: Integer,val)
            Just 4 ->liftIO $ execute conn "UPDATE customers SET email=upd.uemail FROM (VALUES (?,?)) as upd(uid,uemail) WHERE id = upd.uid" (newid:: Integer,val)
            Just 5 ->liftIO $ execute conn "UPDATE customers SET phone=upd.uphone FROM (VALUES (?,?)) as upd(uid,uphone) WHERE id = upd.uid" (newid:: Integer,val)
            Just 6 ->liftIO $ execute conn "UPDATE customers SET client_id=upd.clid FROM (VALUES (?,?)) as upd(uid,clid) WHERE id = upd.uid" (newid:: Integer,read val :: Integer)
            Just 7 ->liftIO $ execute conn "UPDATE customers SET no_of_bookings=upd.nob FROM (VALUES (?,?)) as upd(uid,nob) WHERE id = upd.uid" (newid:: Integer,read val :: Integer)
            _ -> pure 0
        _-> pure 0
    if res > 0 then
        liftIO $ putStrLn "Customer Updated"
    else
        liftIO $ putStrLn "Update failed"

customerUpdateAll :: Maybe Integer -> ReaderT Connection IO()
customerUpdateAll nid = 
    case nid of
        Just newid ->  do
            conn <- ask
            newCustomer <- liftIO $ setCustomer
            Customer{custId =_,custEmail =ce,custName =cn,custReff =cr,custTitle =ct,custPhone =cp,custClId =cl,custnBook =nb,custCreatedAt =ca,custUpdatedAt =ua } <- liftIO $ setCustId newid newCustomer
            res <- 
                liftIO $ execute conn "UPDATE customers SET customer_ref=upd.cref,title=upd.utitle,full_name=upd.ufname,email=upd.uemail,phone=upd.uphone,client_id=upd.clid,number_of_bookings=upd.nob,created_at=upd.cat,updated_at=uat FROM (VALUES (?,?,?,?,?,?,?,?,?,?)) as upd(uid,cref,utitle,ufname,uemail,uphone,clid,nob,cat,uat) WHERE id = upd.uid" (newid,cr,ct,cn,ce,cp,cl::Integer,nb::Integer,ca,ua)
            if res > 0 then
                liftIO $ putStrLn "Customer Updated"
            else
                liftIO $ putStrLn "Update failed"
        _-> liftIO $ putStrLn "Update failed"

deleteCustomer :: ReaderT Connection IO()
deleteCustomer = do
    conn <- ask
    liftIO $ putStr "Enter id of customer to delete: "
    i <- liftIO $ getLine
    res <- case (readMaybe i :: Maybe Integer) of
        Just v -> liftIO $ execute conn "DELETE FROM customers WHERE id = (?)" [v]
        _ -> pure 0
    if res > 0 then
        liftIO $ putStrLn "Customer Deleted"
    else
        liftIO $ putStrLn "Delete failed"

consoleDb :: IO()
consoleDb = do
    putStrLn "Enter 1 to Insert\nEnter 2 to Update\nEnter 3 to Delete\nEnter 4 to View\nEnter 0 to Exit\n"
    ch <- getLine
    conn <- dbConnection
    case ((readMaybe ch)::Maybe Int) of
        Just 1 -> runReaderT insertCustomer conn
        Just 2 -> runReaderT updateCustomer conn
        Just 3 -> runReaderT deleteCustomer conn
        Just 4 -> runReaderT displayCustomers conn
        Just 0 -> putStrLn "Exiting..."
        _ -> putStrLn "Invalid choice" >> consoleDb