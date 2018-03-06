module Monads.User where

import Data.List
import Text.Read
data Email = MkEmail String deriving (Eq, Show, Ord)
data Status = Active | Inactive | Deactive deriving (Eq, Show, Ord)

data User = MkUser
  { userEmail :: Email
  , userFullName :: String
  , userPassword :: String
  , userPostalCode :: String
  , userStatus :: Status
  , userVerificationCode :: String
  } deriving (Eq, Show, Ord)

data NewUser = MkNewUser
  { nuserEmail :: Email
  , nuserFullName :: String
  , nuserPassword :: String
  , nuserPostalCode :: String
  } deriving (Eq, Show, Ord)

registerUser :: NewUser -> [User] -> [User]
registerUser newUser@MkNewUser{nuserEmail=(MkEmail email)} userDb =
  let user = MkUser
              { userEmail = (nuserEmail newUser)
              , userFullName = (nuserFullName newUser)
              , userPassword = (nuserPassword newUser)
              , userPostalCode = (nuserPostalCode newUser)
              , userStatus = Inactive
              , userVerificationCode = (take 2 email) ++ (take 2 (nuserFullName newUser)) ++ (take 2 (nuserPostalCode newUser))
              }
  in (user:userDb)

verifyUser :: Email -> String -> [User] -> (Bool, String, [User])
verifyUser e code userDb =
  let existingUsers = Data.List.filter (\u -> (userEmail u) == e) userDb
  in  if (Data.List.null existingUsers)
        then (False, "No such user", userDb)
        else  let existingUser = head existingUsers
              in  if (code==(userVerificationCode existingUser))
                    then  let verifiedUser = existingUser{userStatus=Active}
                              newUserDb = replaceUserInDb verifiedUser userDb
                          in  (True, "Verified", newUserDb)
                    else (False, "Incorrect verification code", userDb)

deactivateUser :: Email -> [User] -> (Bool, String, [User])
deactivateUser e userDb =
  let existingUsers = Data.List.filter (\u -> (userEmail u) == e) userDb
  in  if (Data.List.null existingUsers)
        then (False, "No such user", userDb)
        else  let existingUser = head existingUsers
                  deactiveUser = existingUser{userStatus = Deactive}
              in  (True, "User deactivated", replaceUserInDb deactiveUser userDb)

replaceUserInDb :: User -> [User] -> [User]
replaceUserInDb u userDb =
  let (a, b) = Data.List.break (\x -> (userEmail x)==(userEmail u)) userDb
  in if (Data.List.null b)
      then  (a ++ [u])
      else  (a ++ (u:(tail b)))

countUsers :: Status -> [User] -> Int
countUsers status userDb = length (filter (\u -> (userStatus u) == status) userDb)

userStatusSummary :: [User] -> [(Status, Int)]
userStatusSummary userDb =
  let statuses = [Active, Inactive, Deactive]
  in map (\status -> (status, countUsers status userDb)) statuses

getRegisterUser :: [User] -> IO[User]
getRegisterUser usrDb = putStrLn "Enter Email: " >> getLine >>= \email -> 
  putStrLn "Enter FullName: " >> getLine >>= \fname -> 
    putStrLn "Enter Password: " >> getLine >>= \pass -> 
      putStrLn "Enter PostalCode: " >> getLine >>= \pcode -> 
              let newUsr = MkNewUser { nuserEmail = MkEmail email
                , nuserFullName = fname
                , nuserPassword = pass
                , nuserPostalCode = pcode
                }
              in pure (registerUser newUsr usrDb)

getVerifyUser:: [User] -> IO[User]
getVerifyUser usrDb = putStrLn "Enter Email: " >> getLine >>= \email -> 
  putStrLn "Enter Verification code: " >> getLine >>= \code -> 
       let (_,_,usrList) = verifyUser (MkEmail email) code usrDb
       in pure usrList

getDeactivateUser :: [User] -> IO[User]
getDeactivateUser usrDb = putStrLn "Enter Email: " >> getLine >>= \email -> 
       let (_,_,usrList) = deactivateUser (MkEmail email) usrDb
       in pure usrList

getReplaceUser :: [User] -> IO[User]
getReplaceUser usrDb = putStrLn "Enter Email: " >> getLine >>= \email -> 
  putStrLn "Enter FullName: " >> getLine >>= \fname -> 
    putStrLn "Enter Password: " >> getLine >>= \pass -> 
      putStrLn "Enter PostalCode: " >> getLine >>= \pcode -> 
              let newUsr = MkUser { userEmail = MkEmail email
                , userFullName = fname
                , userPassword =pass
                , userPostalCode = pcode
                }
              in pure (replaceUserInDb newUsr usrDb)

getCountUser :: Status -> [User] -> IO()
getCountUser status usrDb = 
   case status of
        Active -> putStrLn (show (countUsers Active usrDb))
        Inactive -> putStrLn (show (countUsers Deactive usrDb))
        Deactive -> putStrLn (show (countUsers Inactive usrDb))

displayDb :: [User] -> IO()
displayDb usrDb =
  let opStr = foldl' (\op MkUser{userEmail=MkEmail email,userFullName = fname,userPassword = passwd,userPostalCode = pcode,userStatus = status,userVerificationCode = vcode} ->
        op++" | "++email++" | "++fname++" | "++passwd++" | "++pcode++" | "++(show status)++" | "++vcode++"|\n") "" usrDb
  in putStrLn opStr

displayUsers :: [User] -> IO()
displayUsers usrDb = 
  (putStrLn "1 to Register\n2 to Verify\n3 to Deactivate\n4 to Replace\n5 to Count Active users\n6 to Count Inctive users\n7 to Count Deactive users\n0 to Display\n") >>
    getLine >>= \ ch ->
          case ((readMaybe ch)::Maybe Int) of
            Just 1 -> getRegisterUser usrDb >>= displayUsers
            Just 2-> getVerifyUser usrDb >>= displayUsers
            Just 3-> getDeactivateUser usrDb >>= displayUsers
            Just 4-> getReplaceUser usrDb >>= displayUsers
            Just 5-> getCountUser Active usrDb >> displayUsers usrDb
            Just 6-> getCountUser Inactive usrDb >> displayUsers usrDb
            Just 7-> getCountUser Deactive usrDb  >> displayUsers usrDb
            Just 0-> displayDb usrDb
            _-> putStrLn "Invalid Choice" >> displayUsers usrDb