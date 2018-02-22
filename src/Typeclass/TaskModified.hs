module Typeclass.Task2 where

import Data.List as DL
import Data.Map.Strict as Map

data Permission = Permission
  { permissionId :: Int
  , permissionAction :: String
  , permissionClass :: String
  , permissionDescripton :: String
  } deriving (Eq, Show, Ord)
data Role = Role
  { roleId :: Int
  , roleName :: String
  } deriving (Eq, Show, Ord)
data User = User
  { userId :: Int
  , userEmail :: String
  } deriving (Eq, Show, Ord)

type UITable = Map.Map User ([Permission], Map.Map Role [Permission])

--[(userId,email,permissionId,action,class,description,roleId,roleName,permissionId,action,class,description)]
joinResults :: [(Int,String,Maybe Int,Maybe String,Maybe String,Maybe String,Maybe Int,Maybe String,Maybe Int,Maybe String,Maybe String,Maybe String)]
joinResults = [(1,"abc@abc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(2,"bbc@bc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(3,"abc@abc.com",Nothing,Nothing,Nothing,Nothing,Just 1,Just "Reservation Manager",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar")]

prepareUITable :: UITable
prepareUITable = Map.fromList (DL.map (\(uId,email,ipermid,ipermact,ipermcls,ipermdes,rolid,rolname,_,_,_,_)->( --outer map
  --attach user object
  User {userId=uId,userEmail=email},(
    --permission list
    case ipermid of
      Nothing -> []
      --iterate through indv permissions
      Just cipermid -> let
          Just cipermact=ipermact
          Just cipermcls=ipermcls
          Just cipermdes=ipermdes
        in [Permission {permissionId=cipermid,permissionAction=cipermact,permissionClass=cipermcls,permissionDescripton=cipermdes}]
    ,case rolid of
      Nothing -> Map.fromList []
      --inner map
      --iterate through roles
      Just crlid -> let 
            Just crlname=rolname
        in  Map.fromList [(Role {roleId=crlid,roleName=crlname},DL.foldl' (\arr y-> --iterate through permissions in role
                --permissions for roles
                case y of 
                --check if permission exists each record
                (ccuid,_,_,_,_,_,Just ccrlid,_,Just ccrpermid,Just ccrpermact,Just ccrpermcls,Just ccrpermdes)->
                    --match permission ids to get record
                    if uId == ccuid && ccrlid == crlid
                    --attach permission list for the role
                    then arr++ [Permission {permissionId=ccrpermid,permissionAction=ccrpermact,permissionClass=ccrpermcls,permissionDescripton=ccrpermdes}]
                    else arr
                _->arr
                ) []
            joinResults)]

      )
    )) joinResults)

displayUITable :: UITable -> [(String, ([String], [(String, [String])]))]
displayUITable uiTable = DL.map (\(k,v)->(
    let User {userId=_,userEmail=email} = k
    in email
    ,
    let 
      (inperm,roles) = v
      ipermList = DL.map (\Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pdes}->pdes) inperm
    in (ipermList,
      DL.map (\(Role{roleId=_,roleName=rolname},rolperm)->(rolname,
      DL.map (\Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pdes}->pdes) rolperm
      )) (toList roles)
    )
  )) (toList uiTable)