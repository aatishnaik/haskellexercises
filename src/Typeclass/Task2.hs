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
prepareUITable = Map.fromList (DL.map (\(uId,email,ipermid,_,_,_,rolid,_,_,_,_,_)->(
  User {userId=uId,userEmail=email},(
    case ipermid of
      Nothing -> []
      Just indvpermId -> DL.foldl' (\arr x -> 
          case x of
            (cuid,_,Just cipermid,Just cipermact,Just cipermcls,Just cipermdes,_,_,_,_,_,_)->
              if indvpermId == cipermid && uId == cuid
              then arr++[Permission {permissionId=cipermid,permissionAction=cipermact,permissionClass=cipermcls,permissionDescripton=cipermdes}]
              else arr
            _-> arr
        ) [] joinResults,
    case rolid of
      Nothing -> Map.fromList []
      Just roleid -> Map.fromList (DL.foldl' (\oarr x ->
        case x of
        (cuid,_,_,_,_,_,Just crlid,Just crlname,_,_,_,_) ->
          if uId == cuid && roleid == crlid
          then oarr++[(Role {roleId=crlid,roleName=crlname},DL.foldl' (\arr y->
            case y of 
              (ccuid,_,_,_,_,_,Just ccrlid,_,Just ccrpermid,Just ccrpermact,Just ccrpermcls,Just ccrpermdes)->
                if uId == cuid && cuid == ccuid && roleid==crlid && ccrlid == crlid
                  then arr++ [Permission {permissionId=ccrpermid,permissionAction=ccrpermact,permissionClass=ccrpermcls,permissionDescripton=ccrpermdes}]
                  else arr
              _->arr
              ) []
          joinResults)]
          else oarr
        _->oarr
        ) [] joinResults)
      )
    )) joinResults)