{-# LANGUAGE OverloadedStrings #-}
module Database.Consoledb where
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Data.Time.LocalTime
import Data.List
import Text.Read
import GHC.Int
import Prelude hiding (id)
data Customer = Customer {
    cId :: Maybe Integer,
    cEmail :: String,
    cName :: String,
    cReff :: String,
    cTitle :: String,
    cPhone :: String,
    clId :: Integer,
    nBook :: Integer,
    createdAt :: LocalTime,
    updatedAt :: LocalTime
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
        pure Customer{cId=Just id,cEmail=email,cName=full_name,cReff=customer_ref,cTitle=title,cPhone=phone,clId=client_id,nBook=number_of_bookings,createdAt=created_at,updatedAt=updated_at}

instance ToRow Customer where
    toRow r = [toField (cEmail r),toField (cName r),toField (cReff r),toField (cTitle r),toField (cPhone r),toField (clId r),toField (nBook r),toField (createdAt r),toField (updatedAt r)]

dbConnection :: IO Connection
dbConnection = connect ConnectInfo{connectHost ="localhost",connectPort = 5432,connectUser ="b2b",connectPassword ="b2b",connectDatabase ="b2b"}

displayCustomer :: IO ()
displayCustomer = do
    conn <- dbConnection
    list <- query_ conn "SELECT id,customer_ref,title,full_name,email,phone,client_id,number_of_bookings,created_at,updated_at FROM customers" :: IO [Customer]
    --pure list
    opStr <- pure (foldl' (\arr Customer{cId=_,cEmail=e,cName=n,cReff=_,cTitle=_,cPhone=_,clId=_,nBook=_,createdAt=_,updatedAt=_} -> arr++n++" "++e++"\n") "Customer List:\n" list)
    putStrLn opStr

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
    cid <- getLine
    putStr "Enter No of Bookings: "
    nob <- getLine
    pure Customer{cId=Nothing,cEmail=em,cName=fn,cReff=cref,cTitle=tl,cPhone=ph,clId=(read cid):: Integer,nBook=(read nob):: Integer,createdAt=(read "2018-04-09 17:21:49.921882" :: LocalTime),updatedAt=(read "2018-04-09 17:21:49.921882" :: LocalTime)}
setCustId :: Integer -> Customer -> IO Customer
setCustId i Customer{cId=_,cEmail=em,cName=fn,cReff=cref,cTitle=tl,cPhone=ph,clId=cid,nBook=nob,createdAt=ca,updatedAt=ua} = pure Customer{cId=Just i,cEmail=em,cName=fn,cReff=cref,cTitle=tl,cPhone=ph,clId=cid,nBook=nob,createdAt=ca,updatedAt=ua}
insertCustomer :: IO ()
insertCustomer = do
    conn <- dbConnection
    customer <- setCustomer
    --Customer{cId=_,cEmail=email,cName=fullName,cReff=customerRef,cTitle=title,cPhone=phone,clId=clientId,nBook=noOfBookings} <- getCustomer
    res <- execute conn "INSERT INTO customers (customer_ref,title,full_name,email,phone,client_id,number_of_bookings,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?)" customer--[(customerRef,title,fullName,email,phone,clientId,noOfBookings,"2018-04-09 17:21:49.921882","2018-04-09 17:21:49.921882")]
    if res > 0 then
        putStrLn "Customer Inserted"
    else
        putStrLn "Insertion failed"

updateCustomer :: IO ()
updateCustomer = do
    putStrLn "Enter 1 to update single value\nEnter 2 to update entire customer\nEnter 0 to cancel update\n"
    ch <- getLine
    putStr "Enter Id of customer to update: "
    i <- getLine
    conn <- dbConnection
    case ((readMaybe ch)::Maybe Int) of
        Just 1 -> customerUpdateOne ((read i)::Integer) conn
        Just 2 -> customerUpdateAll ((read i)::Integer) conn
        Just 0 -> putStrLn "Update cancelled.."
        _ -> putStrLn "Invalid choice" >> updateCustomer

customerUpdateOne :: Integer -> Connection -> IO()
customerUpdateOne newid conn= do
    putStrLn "Enter 1 to update Customer Ref\nEnter 2 to update Title\nEnter 3 to update Fullname\nEnter 4 to update Email\nEnter 5 to update Phone\nEnter 6 to update Client Id\nEnter 7 to update Bookings\nEnter 0 to Cancel\n"
    ch <- getLine
    putStr "Enter New Value: "
    val <- getLine
    res <- case (readMaybe ch :: Maybe Int) of
        Just 1 ->execute conn "UPDATE customers SET cref=upd.customer_ref FROM (VALUES (?,?)) as upd(uid,cref) WHERE id = upd.uid" (newid:: Integer,val)
        Just 2 ->execute conn "UPDATE customers SET title=upd.utitle FROM (VALUES (?,?)) as upd(uid,utitle) WHERE id = upd.uid" (newid:: Integer,val)
        Just 3 ->execute conn "UPDATE customers SET full_name=upd.ufname FROM (VALUES (?,?)) as upd(uid,ufname) WHERE id = upd.uid" (newid:: Integer,val)
        Just 4 ->execute conn "UPDATE customers SET email=upd.uemail FROM (VALUES (?,?)) as upd(uid,uemail) WHERE id = upd.uid" (newid:: Integer,val)
        Just 5 ->execute conn "UPDATE customers SET phone=upd.uphone FROM (VALUES (?,?)) as upd(uid,uphone) WHERE id = upd.uid" (newid:: Integer,val)
        Just 6 ->execute conn "UPDATE customers SET client_id=upd.clid FROM (VALUES (?,?)) as upd(uid,clid) WHERE id = upd.uid" (newid:: Integer,read val :: Integer)
        Just 7 ->execute conn "UPDATE customers SET no_of_bookings=upd.nob FROM (VALUES (?,?)) as upd(uid,nob) WHERE id = upd.uid" (newid:: Integer,read val :: Integer)
        _ -> pure 0
    if res > 0 then
        putStrLn "Customer Updated"
    else
        putStrLn "Update failed"

customerUpdateAll :: Integer -> Connection -> IO()
customerUpdateAll newid conn = do
    newCustomer <- setCustomer
    Customer{cId =_,cEmail =ce,cName =cn,cReff =cr,cTitle =ct,cPhone =cp,clId =cl,nBook =nb,createdAt =ca,updatedAt =ua } <- setCustId newid newCustomer
    res <- execute conn "UPDATE customers SET customer_ref=upd.cref,title=upd.utitle,full_name=upd.ufname,email=upd.uemail,phone=upd.uphone,client_id=upd.clid,number_of_bookings=upd.nob,created_at=upd.cat,updated_at=uat FROM (VALUES (?,?,?,?,?,?,?,?,?,?)) as upd(uid,cref,utitle,ufname,uemail,uphone,clid,nob,cat,uat) WHERE id = upd.uid" (newid,cr,ct,cn,ce,cp,cl::Integer,nb::Integer,ca,ua)
    if res > 0 then
        putStrLn "Customer Updated"
    else
        putStrLn "Update failed"

deleteCustomer :: IO ()
deleteCustomer = do
    conn <- dbConnection
    putStr "Enter id of customer to delete: "
    i <- getLine
    res <- execute conn "DELETE FROM customers WHERE id = (?)" [read i :: Integer]
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