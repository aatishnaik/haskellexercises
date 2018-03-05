module Monads.User where

import Data.List

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

getRegisterUser :: [User] -> IO()
getRegisterUser usrDb = getLine >>= \email -> 
  getLine >>= \fname -> 
    getLine >>= \pass -> 
      getLine >>= \pcode -> 
              let newUsr = MkNewUser { nuserEmail = MkEmail email
                , nuserFullName = fname
                , nuserPassword = pass
                , nuserPostalCode = pcode
                }
              in displayUsers (registerUser newUsr usrDb)

getVerifyUser:: [User] -> IO()
getVerifyUser usrDb = getLine >>= \email -> 
  getLine >>= \code -> 
       let (_,_,usrList) = verifyUser (MkEmail email) code usrDb
       in displayUsers usrList

getDeactivateUser :: [User] -> IO()
getDeactivateUser usrDb = getLine >>= \email -> 
       let (_,_,usrList) = deactivateUser (MkEmail email) usrDb
       in displayUsers usrList

getReplaceUser :: [User] -> IO()
getReplaceUser usrDb = getLine >>= \email -> 
  getLine >>= \fname -> 
    getLine >>= \pass -> 
      getLine >>= \pcode -> 
              let newUsr = MkUser { userEmail = MkEmail email
                , userFullName = fname
                , userPassword =pass
                , userPostalCode = pcode
                }
              in displayUsers (replaceUserInDb newUsr usrDb)

getCountUser :: Status -> [User] -> IO()
getCountUser status usrDb = 
   case status of
        Active -> putStrLn (show (countUsers Active usrDb))
        Inactive -> putStrLn (show (countUsers Deactive usrDb))
        Deactive -> putStrLn (show (countUsers Inactive usrDb))

displayDb :: [User] -> IO()
displayDb usrDb =
  let opStr = foldl' (\op MkUser{userEmail=MkEmail email,userFullName = fname,userPassword = passwd,userPostalCode = pcode,userStatus = status,userVerificationCode = vcode} ->
        op++" "++email++" "++fname++" "++passwd++" "++pcode++" "++(show status)++" "++vcode++"\n") "" usrDb
  in putStrLn opStr

displayUsers :: [User] -> IO()
displayUsers usrDb = inpCh
  --(>>=) (pure "1 to Register\n2 to Verify\n3 to Deactivate\n4 to Replace\n5 to Count Active users\n6 to Count Inctive users\n7 to Count Deactive users\n0 to Display\n") inpCh
                where 
                  inpCh = getLine >>= \ ch ->
                    case ch of
                      "1"-> getRegisterUser usrDb
                      "2"-> getVerifyUser usrDb
                      "3"-> getDeactivateUser usrDb
                      "4"-> getReplaceUser usrDb
                      "5"-> getCountUser Active usrDb
                      "6"-> getCountUser Inactive usrDb
                      "7"-> getCountUser Deactive usrDb
                      "0"-> displayDb usrDb
                      _-> putStrLn "Invalid Choice"