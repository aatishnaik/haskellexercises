module Typeclass.New where

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
joinResults :: [(Int,String,Maybe Int,Maybe String,Maybe String,Maybe String,Maybe Int,Maybe String,Maybe Int,Maybe String,Maybe String,Maybe String)]
joinResults = [(1,"abc@abc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(1,"abc@abc.com",Just 1,Just "Reserve Bus",Just "Trips::Trip",Just "Allowed to edit Bus",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(2,"bbc@bc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(3,"ccc@abc.com",Nothing,Nothing,Nothing,Nothing,Just 1,Just "Reservation Manager",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar"),(3,"ccc@abc.com",Nothing,Nothing,Nothing,Nothing,Just 2,Just "Reservation Manager",Just 2,Just "manage_Bus",Just "Trips::Trip",Just "Allowed to edit Bus")]

prepareUITable :: UITable
prepareUITable = DL.foldl' (\accMap (usrId,usrEmail,_,_,_,_,_,_,_,_,_,_)-> 
    let indvP= DL.foldl' (\iperms (_,_,Just fiperId,Just fiperAct,Just fiperCls,Just fiperDes,_,_,_,_,_,_)->
                            iperms++[(Permission {permissionId = fiperId,permissionAction=fiperAct,permissionClass=fiperCls,permissionDescripton=fiperDes})]
                        ) [] (DL.filter (\(fuid,_,fpid,fpact,fpcls,fpdes,_,_,_,_,_,_)->fuid==usrId && 
                            case (fpid,fpact,fpcls,fpdes) of
                                (Just _,Just _,Just _,Just _) -> True
                                _ -> False
                        ) joinResults)
        rolP= DL.foldl' (\rperms (_,_,_,_,_,_,Just frolId,Just frolName,Just _,Just _,Just _,Just _)->
                    Map.insert (Role {roleId = frolId,roleName = frolName}) (
                        DL.foldl' (\rolperms (_,_,_,_,_,_,_,_,Just ffpid,Just ffpact,Just ffpcls,Just ffpdes)->
                                rolperms ++ [Permission {permissionId=ffpid,permissionAction=ffpact,permissionClass=ffpcls,permissionDescripton=ffpdes}]
                            ) [] (DL.filter (\(fuid,_,_,_,_,_,frid,frname,fpid,fpact,fpcls,fpdes)->fuid==
                            usrId && 
                    case (Just frid,Just frname,Just fpid,Just fpact,Just fpcls,Just fpdes) of
                        (Just rrrid,Just _,Just _,Just _,Just _,Just _) -> 
                            if rrrid == frid then True else False
                        _ -> False
                    ) joinResults)
                    ) rperms
                ) Map.empty (DL.filter (\(fuid,_,_,_,_,_,frid,frname,fpid,fpact,fpcls,fpdes)->fuid==usrId && 
                    case (frid,frname,fpid,fpact,fpcls,fpdes) of
                        (Just _,Just _,Just _,Just _,Just _,Just _) -> True
                        _ -> False
                    ) joinResults)

    in Map.insert (User {userId=usrId,userEmail=usrEmail}) (
            (indvP,rolP)
        ) accMap 
    ) Map.empty joinResults

displayUITable :: UITable -> String
displayUITable uiTable = DL.foldl' (\opstr (k,v)->(
    let User {userId=_,userEmail=email} = k
    in opstr++(fixStr email 20)++
    let 
      (inperm,roles) = v
      ipermList = DL.foldl' (\ppstr Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pdes}->ppstr++(fixStr pdes 40)++"\n") "" inperm
    in 
      DL.foldl' (\rstr (Role{roleId=_,roleName=rolname},rolperm)->(rstr++(fixStr rolname 30)++
      DL.foldl' (\pstr Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pdes}->(pstr++(fixStr pdes 40))) "" rolperm
      )) "" (toList roles)++ipermList
    )
  ) "" (toList uiTable)

showUITable :: UITable -> [(String, ([String], [(String, [String])]))]
showUITable uiTable = DL.map (\(k,v)->(
    let User {userId=_,userEmail=email} = k
    in (fixStr email 20)
    ,
    let 
      (inperm,roles) = v
      ipermList = DL.map (\Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pdes}->(fixStr pdes 40)) inperm
    in (ipermList,
      DL.map (\(Role{roleId=_,roleName=rolname},rolperm)->((fixStr rolname 30),
      DL.map (\Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pdes}->(fixStr pdes 40)) rolperm
      )) (toList roles)
    )
  )) (toList uiTable)

fixStr :: String -> Int -> String
fixStr str n = setsp str (n - (length str))
        where setsp st i = if i<=0
                            then st
                            else setsp (st ++ " ") (i-1) 