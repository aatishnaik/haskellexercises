module Monads.User2 where
import Data.Char
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

specExpList :: String
specExpList = "!@#$%^&*()_?{}[]><.:;|-+=~'`~"

registerUser :: NewUser -> [User] -> Either String [User]
registerUser newUser@MkNewUser{nuserEmail=(MkEmail email)} userDb =
  let 
    user = MkUser
              { userEmail = (nuserEmail newUser)
              , userFullName = (nuserFullName newUser)
              , userPassword = (nuserPassword newUser)
              , userPostalCode = (nuserPostalCode newUser)
              , userStatus = Inactive
              , userVerificationCode = (take 2 email) ++ (take 2 (nuserFullName newUser)) ++ (take 2 (nuserPostalCode newUser))
              }
    existingUsers = Data.List.filter (\u -> (userEmail u) == (userEmail user)) userDb
  in if (Data.List.null existingUsers) then Right (user:userDb) else Left "Username already exists TRY AGAIN\n"

verifyUser :: Email -> String -> [User] -> (Either String [User])
verifyUser e code userDb =
  let existingUsers = Data.List.filter (\u -> (userEmail u) == e) userDb
  in  if (Data.List.null existingUsers)
        then Left "No such user exists TRY AGAIN\n"
        else  let existingUser = head existingUsers
              in  if (code==(userVerificationCode existingUser))
                    then  let verifiedUser = existingUser{userStatus=Active}
                              newUserDb = replaceUserInDb verifiedUser userDb
                          in  Right newUserDb
                    else Left "Incorrect verification code TRY AGAIN\n"

deactivateUser :: Email -> [User] -> (Either String [User])
deactivateUser e userDb =
  let existingUsers = Data.List.filter (\u -> (userEmail u) == e) userDb
  in  if (Data.List.null existingUsers)
        then Left "No such user exists TRY AGAIN\n"
        else  let existingUser = head existingUsers
                  deactiveUser = existingUser{userStatus = Deactive}
              in  Right (replaceUserInDb deactiveUser userDb)

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

getRegisterUser :: [User] -> IO([User])
getRegisterUser usrDb = validateEmailId "" 0 >>= \email -> 
  validateName "" 0 >>= \fname -> 
    validatePassword "" 0 >>= \pass -> 
      validatePostalCode "" 0 >>= \pcode -> 
        let 
            newUsr = MkNewUser { nuserEmail = MkEmail email
                , nuserFullName = fname
                , nuserPassword = pass
                , nuserPostalCode = pcode
                }
            regOp = registerUser newUsr usrDb
        in case regOp of
            Left err -> putStrLn err >> getRegisterUser usrDb
            Right usr -> pure (usr)

getVerifyUser:: [User] -> IO[User]
getVerifyUser usrDb = validateEmailId "" 0 >>= \email -> 
    validateVcode "" 0 >>= \code -> 
       let verifyOp = verifyUser (MkEmail email) code usrDb
       in case verifyOp of
        Right usr -> pure usr
        Left err -> putStrLn err >> getVerifyUser usrDb

getDeactivateUser :: [User] -> IO[User]
getDeactivateUser usrDb = validateEmailId "" 0 >>= \email -> 
       let deactOp = deactivateUser (MkEmail email) usrDb
       in case deactOp of
        Right usr -> pure usr
        Left err -> putStrLn err >> getDeactivateUser usrDb

getReplaceUser :: [User] -> IO[User]
getReplaceUser usrDb = validateEmailId "" 0 >>= \email -> 
  validateName "" 0 >>= \fname -> 
    validatePassword "" 0 >> getLine >>= \pass -> 
      validatePostalCode "" 0 >> getLine >>= \pcode -> 
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


validateEmailId :: String -> Int -> IO(String)
validateEmailId email n
    |n == 0=
      putStrLn "Enter Email:" >> getLine >>= \e -> validateEmailId e (n+1)
    |n < 4=
      let 
        (a1,a2) = (break (=='@') email)
        (d1,d2) = (break (=='.') a2)
      in if (length a1 > 1 && length a2 > 1 && length d1 > 1 && length d2 > 1)
        then pure email
        else putStrLn "Invalid Email ENTER AGAIN" >> getLine >>= \e -> validateEmailId e (n+1)
    |otherwise=
      error "Invalid EmailId entered.\n"

validatePassword :: String -> Int -> IO(String)
validatePassword passwd n
    |passwd == "" && n == 0=
      putStrLn "Enter Password:" >> getLine >>= \pass -> validatePassword pass (n+1)
    |n <= 5=
      let flags=  if (length passwd) > 6
          then (foldl' (\(flow,fupp,fdig,fspec,flen) c-> --iterating char by char in password checking
                if isLower c && flow==False
                  then (True,fupp,fdig,fspec,flen)
                else if isUpper c && fupp==False
                  then (flow,True,fdig,fspec,flen)
                else if isDigit c && fdig==False
                  then (flow,fupp,True,fspec,flen)
                else if (c `elem` specExpList) && fspec==False--checks in list of spec chars
                  then (flow,fupp,fdig,True,flen)
                else (flow,fupp,fdig,fspec,flen)
            ) (False,False,False,False,True) passwd)
            else (False,False,False,False,False)
      in 
        case flags of
          (True,True,True,True,True) -> pure passwd
          (_,_,_,_,False)-> putStrLn "Password should be of length greater than 6 \n" >> getLine >>= \pass -> validatePassword pass (n+1)
          (False,_,_,_,True)-> putStrLn "Password should contain LowerCase Letter \n" >> getLine >>= \pass -> validatePassword pass (n+1)
          (True,False,_,_,True)-> putStrLn "Password should contain UpperCase Letter \n" >> getLine >>= \pass -> validatePassword pass (n+1)
          (True,True,False,_,True)-> putStrLn "Password should contain a Digit \n" >> getLine >>= \pass -> validatePassword pass (n+1)
          (True,True,True,False,True)-> putStrLn "Password should contain a Special character \n" >> getLine >>= \pass -> validatePassword pass (n+1)
    |otherwise=
      error "Invalid Password entered multiple times.\n"

validateName :: String -> Int -> IO(String)
validateName name n
    |name == "" && n == 0=
      putStrLn "Enter Full Name:" >> getLine >>= \nme -> validateName nme (n+1)
    |n < 4=
      let (s1,s2)= break (==' ') name
      in if (length s1 > 1 && length s2 > 1)
        then pure name
      else putStrLn "FuLLName Invalid ENTER AGAIN\n" >> getLine >>= \nme -> validateName nme (n+1)
    |otherwise =
      error "Invalid FullName entered.\n"

validatePostalCode :: String -> Int -> IO(String)
validatePostalCode pcode n
    |n == 0=
      putStrLn "Enter Postal code:" >> getLine >>= \pc -> validatePostalCode pc (n+1)
    |n < 4=
        if ((filter (\c -> not (isDigit c)) pcode) == "") && (length pcode == 6)
        then pure pcode
        else putStrLn "Postal code Invalid ENTER AGAIN\n" >> getLine >>= \pc -> validatePostalCode pc (n+1)
    |otherwise=
        error "Invalid PostalCode entered.\n"

validateVcode :: String -> Int -> IO(String)
validateVcode vcode n
    |n == 0=
      putStrLn "Enter Verification code:" >> getLine >>= \vc -> validateVcode vc (n+1)
    |vcode == "" && n < 4=
      putStrLn "Verification code blank ENTER AGAIN\n" >> getLine >>= \vc -> validateVcode vc (n+1)
    |n > 4=
         error "Invalid VerificationCode entered.\n"
    |otherwise= pure vcode

displayUsers :: [User] -> IO()
displayUsers usrDb = 
  (putStrLn "1 to Register\n2 to Verify\n3 to Deactivate\n4 to Replace\n5 to Count Active users\n6 to Count Inctive users\n7 to Count Deactive users\n0 to Display\n") >>
    getLine >>= \ ch ->
          case ((readMaybe ch)::Maybe Int) of
            Just 1-> getRegisterUser usrDb >>= displayUsers
            Just 2-> getVerifyUser usrDb >>= displayUsers
            Just 3-> getDeactivateUser usrDb >>= displayUsers
            Just 4-> getReplaceUser usrDb >>= displayUsers
            Just 5-> getCountUser Active usrDb >> displayUsers usrDb
            Just 6-> getCountUser Inactive usrDb >> displayUsers usrDb
            Just 7-> getCountUser Deactive usrDb  >> displayUsers usrDb
            Just 0-> displayDb usrDb
            _-> putStrLn "Invalid Choice" >> displayUsers usrDb