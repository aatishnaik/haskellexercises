module Monads.UserValid where
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
specExpList = "!@#$%^&*()_? {}[]><.:;|-+=~'`~"

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
  in if (Data.List.null existingUsers) then Right (user:userDb) else Left "Username already exists TRY AGAIN"

verifyUser :: Email -> String -> [User] -> (Either String [User])
verifyUser e code userDb =
  let existingUsers = Data.List.filter (\u -> (userEmail u) == e) userDb
  in  if (Data.List.null existingUsers)
        then Left "No such user exists TRY AGAIN"
        else  let existingUser = head existingUsers
              in  if (code==(userVerificationCode existingUser))
                    then  let verifiedUser = existingUser{userStatus=Active}
                              newUserDb = replaceUserInDb verifiedUser userDb
                          in  Right newUserDb
                    else Left "Incorrect verification code TRY AGAIN"

deactivateUser :: Email -> [User] -> (Either String [User])
deactivateUser e userDb =
  let existingUsers = Data.List.filter (\u -> (userEmail u) == e) userDb
  in  if (Data.List.null existingUsers)
        then Left "No such user exists TRY AGAIN"
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
getRegisterUser usrDb = 
  do
  email <- getValidatedEmailId 0
  fname<-getValidatedName 0
  pass<-getValidatedPassword 0
  pcode<-getValidatedPostalCode 0
  let 
      newUsr = MkNewUser { nuserEmail = MkEmail email
          , nuserFullName = fname
          , nuserPassword = pass
          , nuserPostalCode = pcode
          }
      regOp = registerUser newUsr usrDb
  case regOp of
      Left err -> do
        putStrLn err
        getRegisterUser usrDb
      Right usr -> pure (usr)

getVerifyUser:: [User] -> IO[User]
getVerifyUser usrDb = 
  do
    email<-getValidatedEmailId 0
    code<-getValidatedVcode 0
    let verifyOp = verifyUser (MkEmail email) code usrDb
    case verifyOp of
        Right usr -> pure usr
        Left err -> do
          putStrLn err
          getVerifyUser usrDb

getDeactivateUser :: [User] -> IO[User]
getDeactivateUser usrDb = 
  do
    email<-getValidatedEmailId 0
    let deactOp = deactivateUser (MkEmail email) usrDb
    case deactOp of
        Right usr -> pure usr
        Left err -> do
          putStrLn err
          getDeactivateUser usrDb

getReplaceUser :: [User] -> IO[User]
getReplaceUser usrDb = 
  do
    email<-getValidatedEmailId 0
    fname<-getValidatedName 0
    pass<-getValidatedPassword 0
    pcode<-getValidatedPostalCode 0
    let newUsr = MkUser { userEmail = MkEmail email
        , userFullName = fname
        , userPassword =pass
        , userPostalCode = pcode
        }
    pure (replaceUserInDb newUsr usrDb)

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

--getFunctions
getValidatedEmailId :: Int -> IO(String)
getValidatedEmailId n
    |n < 4 = 
      do
        putStrLn "Enter Email:"
        e<-getLine
        case (validateEmailId e) of
          Right em -> pure em
          Left err -> do
            putStrLn err
            getValidatedEmailId (n+1)
    |otherwise = error "Invalid EmailId entered"

getValidatedPassword :: Int -> IO(String)
getValidatedPassword n
    |n < 5=
      do
        putStrLn "Enter Password:"
        pass<-getLine
        case (validatePassword pass) of
          Right pwd -> pure pwd
          Left err -> do
            putStrLn err
            getValidatedPassword (n+1)
    |otherwise=
      error "Invalid Password entered multiple times.\n"

getValidatedName :: Int -> IO(String)
getValidatedName n
    |n < 4=
      do
        putStrLn "Enter Full Name:"
        nme<-getLine
        case (validateName nme) of
          Right fname -> pure fname
          Left err -> do
            putStrLn err
            getValidatedName (n+1)
    |otherwise =
      error "Invalid FullName entered."

getValidatedPostalCode :: Int -> IO(String)
getValidatedPostalCode n
    |n < 4=
      do
        putStrLn "Enter Postal code:"
        pc<-getLine
        case (validatePostalCode pc) of
          Right pcode -> pure pcode
          Left err -> do
            putStrLn err
            getValidatedPostalCode (n+1)
    |otherwise=
        error "Invalid PostalCode entered.\n"

getValidatedVcode :: Int -> IO(String)
getValidatedVcode n
    |n < 4=
      do
        putStrLn "Verification code blank ENTER AGAIN"
        vc<-getLine
        case (validateVcode vc) of
          Right vcod -> pure vcod
          Left err -> do
            putStrLn err
            getValidatedVcode (n+1)
    |otherwise = error "Invalid VerificationCode entered."

--validation functions
validateEmailId :: String -> Either String String
validateEmailId email = 
  let 
    (a1,a2) = (break (=='@') email)
    (d1,d2) = (break (=='.') a2)
  in if (length a1 > 1 && length a2 > 1 && length d1 > 1 && length d2 > 1)
    then Right email
    else Left "Invalid Email ENTER AGAIN"

validatePassword :: String -> Either String String
validatePassword passwd = 
  let 
    flen= (length passwd) > 6
    flow = any isLower passwd
    fupp = any isUpper passwd
    fdig = any isDigit passwd
    fspec = any ( `elem` specExpList) passwd
  in 
    case (flow,fupp,fdig,fspec,flen) of
    (True,True,True,True,True) -> Right passwd
    (_,_,_,_,False)-> Left "Password should be of length greater than 6"
    (False,_,_,_,True)-> Left "Password should contain LowerCase Letter"
    (True,False,_,_,True)-> Left "Password should contain UpperCase Letter"
    (True,True,False,_,True)-> Left "Password should contain a Digit"
    (True,True,True,False,True)-> Left "Password should contain a Special character"

validateName :: String -> Either String String
validateName name =
    let (s1,s2)= break (==' ') name
    in
      if (length s1 > 1 && length s2 > 1)
        then Right name
      else Left "FuLLName Invalid ENTER AGAIN"

validatePostalCode :: String -> Either String String
validatePostalCode pcode = if ((filter (\c -> not (isDigit c)) pcode) == "") && (length pcode == 6)
  then Right pcode
  else Left "Postal code Invalid ENTER AGAIN"

validateVcode :: String -> Either String String
validateVcode vcode = if vcode == ""
      then Left "Verification code blank ENTER AGAIN"
      else Right vcode

displayUsers :: [User] -> IO()
displayUsers usrDb = 
  do
    putStrLn "1 to Register\n2 to Verify\n3 to Deactivate\n4 to Replace\n5 to Count Active users\n6 to Count Inctive users\n7 to Count Deactive users\n0 to Display"
    ch<-getLine
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