{-# LANGUAGE TemplateHaskell #-}
module UserRegistration where

    import Data.List
    import Control.Lens as CL
    import Text.Read
    data Email = MkEmail String deriving (Eq, Show, Ord)
    
    data User = MkUser
      { _userEmail :: Email
      , _userFullName :: String
      , _userPassword :: String
      , _userPostalCode :: String
      , _userStatus :: String
      , _userVerificationCode :: String
      } deriving (Eq, Show, Ord)
    makeLenses ''User
    
    data NewUser = MkNewUser
      { _nuserEmail :: Email
      , _nuserFullName :: String
      , _nuserPassword :: String
      , _nuserPostalCode :: String
      } deriving (Eq, Show, Ord)
    
    registerUser :: NewUser -> [User] -> [User]
    registerUser newUser@MkNewUser{_nuserEmail=(MkEmail email)} userDb =
      let user = MkUser
                  { _userEmail = (_nuserEmail newUser)
                  , _userFullName = (_nuserFullName newUser)
                  , _userPassword = (_nuserPassword newUser)
                  , _userPostalCode = (_nuserPostalCode newUser)
                  , _userStatus = "unverified"
                  , _userVerificationCode = (take 2 email) ++ (take 2 (_nuserFullName newUser)) ++ (take 2 (_nuserPostalCode newUser))
                  }
      in (user:userDb)
    
    verifyUser :: Email -> String -> [User] -> (Bool, String, [User])
    verifyUser e code userDb =
      let existingUsers = Data.List.filter (\u -> (u^. userEmail) == e) userDb
      in  if (Data.List.null existingUsers)
            then (False, "No such user", userDb)
            else  let existingUser = head existingUsers
                  in  if (code==(_userVerificationCode existingUser))
                        then  let verifiedUser = existingUser{_userStatus="active"}
                                  _newUserDb = replaceUserInDb verifiedUser userDb
                              in  (True, "Verified", _newUserDb)
                        else (False, "Incorrect verification code", userDb)
    
    deactivateUser :: Email -> [User] -> (Bool, String, [User])
    deactivateUser e userDb =
      let existingUsers = Data.List.filter (\u -> (u^. userEmail) == e) userDb
      in  if (Data.List.null existingUsers)
            then (False, "No such user", userDb)
            else  let existingUser = head existingUsers
                      deactiveUser = existingUser{_userStatus = "deactivated"}
                  in  (True, "User deactivated", replaceUserInDb deactiveUser userDb)
    
    replaceUserInDb :: User -> [User] -> [User]
    replaceUserInDb u userDb = let (a,b) = Data.List.break (\x -> (x^. userEmail)==(u^. userEmail)) userDb
      in if (Data.List.null b) then  (a ++ [u]) else  (a ++ (u:(tail b)))