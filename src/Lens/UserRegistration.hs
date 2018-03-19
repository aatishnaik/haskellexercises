module UserRegistration where

    import Data.List (break, null, filter, take)
    
    data Email = MkEmail String deriving (Eq, Show, Ord)
    
    data User = MkUser
      { userEmail :: Email
      , userFullName :: String
      , userPassword :: String
      , userPostalCode :: String
      , userStatus :: String
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
                  , userStatus = "unverified"
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
                        then  let verifiedUser = existingUser{userStatus="active"}
                                  newUserDb = replaceUserInDb verifiedUser userDb
                              in  (True, "Verified", newUserDb)
                        else (False, "Incorrect verification code", userDb)
    
    deactivateUser :: Email -> [User] -> (Bool, String, [User])
    deactivateUser e userDb =
      let existingUsers = Data.List.filter (\u -> (userEmail u) == e) userDb
      in  if (Data.List.null existingUsers)
            then (False, "No such user", userDb)
            else  let existingUser = head existingUsers
                      deactiveUser = existingUser{userStatus = "deactivated"}
                  in  (True, "User deactivated", replaceUserInDb deactiveUser userDb)
    
    replaceUserInDb :: User -> [User] -> [User]
    replaceUserInDb u userDb =
      let (a, b) = Data.List.break (\x -> (userEmail x)==(userEmail u)) userDb
      in if (Data.List.null b)
          then  (a ++ [u])
          else  (a ++ (u:(tail b)))