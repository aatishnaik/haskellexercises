{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Typeclass.NewTest where

import Data.List as DL
import Data.Map.Strict as Map

type UITable = Map.Map User ([Permission], Map.Map Role [Permission])
joinResults :: [(Int,String,Maybe Int,Maybe String,Maybe String,Maybe String,Maybe Int,Maybe String,Maybe Int,Maybe String,Maybe String,Maybe String)]
joinResults = [(1,"abc@abc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(1,"abc@abc.com",Just 1,Just "Reserve Bus",Just "Trips::Trip",Just "Allowed to edit Bus",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(2,"bbc@bc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(1,"abc@abc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(3,"ccc@abc.com",Just 1,Just "Reserve Bus",Just "Trips::Trip",Just "Allowed to edit Bus",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(3,"ccc@abc.com",Nothing,Nothing,Nothing,Nothing,Just 1,Just "Reservation Manager",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar"),(3,"ccc@abc.com",Nothing,Nothing,Nothing,Nothing,Just 2,Just "Reservation Manager",Just 2,Just "manage_Bus",Just "Trips::Trip",Just "Allowed to edit Bus")]

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


displayUITable :: UITable ->[[(String,(String,String),String)]]
displayUITable uiTable = DL.map (\ (email,y)->
        let
            (iperms,rs) = y --pattern match tuple
            --max size of emails
            sizeEmail=DL.foldl' (\m x -> if (length (fst x))>m then length (fst x) else m) 0 (showUITable uiTable) +3
            --max size of perms
            sizePerm=(DL.foldl' (\m (_,(ipermArr,_)) -> 
                let si = DL.foldl' (\m2 x -> if length x > m2 then length x else m2) 0 ipermArr 
                in if si > m then si else m
                ) 0 (showUITable uiTable))+7
            --max size of roles
            sizeRole=DL.foldl' (\m (_, (_,rls)) -> 
                let si = DL.foldl' (\m2 (x,_) -> if length x > m2 then length x else m2) 0 rls
                in if si > m then si else m
                ) 0 (showUITable uiTable)+3
            --number of indv perms
            nmaxI= (nmaxIPerm y)
            --number of role perms
            nmaxR= (nmaxRPerm y)
            -- max of the 2
            nmax = if  nmaxI > nmaxR then nmaxI else nmaxR
            --padded useremails
            usrEmails = [fixStr email sizeEmail] ++ emailSet
                where emailSet=(DL.map (\ _ -> (fixStr "" sizeEmail)) [1..nmax-1])
            --padded indv perms
            usrIPerms = if nmaxI==nmax
                then ipermSet
                else ipermSet ++ (DL.map (\_-> (fixStr "" sizePerm)++"\n") [1..(nmaxR-nmaxI)])
                where ipermSet = DL.map (\x->(fixStr x sizePerm)++"\n") iperms
            --padded roles
            usrR = DL.foldl' (\rList (roleL,rpermsL)->
                    rList ++ rset roleL rpermsL
                ) [] rs
                where rset role rperms=  let 
                                            zrole=[(fixStr role sizeRole)]
                                            spList = DL.map (\_ -> (fixStr "" sizeRole)) [1..(length rperms)]
                                        in zip (zrole ++ spList) rperms
            --padded role perms
            usrRPerms = if nmaxR==nmax
                    then usrR
                    else usrR ++ rpSet
                where rpSet = let arr = [1..(nmaxI-nmaxR)]
                            in DL.map (\_-> ((fixStr "" sizeRole),(fixStr "" sizePerm))) arr
        in zip3 usrEmails usrRPerms usrIPerms --zip all padded columns
    ) (showUITable uiTable)

--extracts needed string field from objects
showUITable :: UITable -> [(String, ([String], [(String, [String])]))]
showUITable uiTable = DL.map (\(k,v)->(
    let User {userId=_,userEmail=email} = k
    in email,
    let 
      (isizePerm,roles) = v
      ipermList = DL.map (\Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pdes}->
        pdes) isizePerm
    in (ipermList,
      DL.map (\(Role{roleId=_,roleName=rolname},rolperm)->(rolname,
      DL.map (\Permission{permissionId=_,permissionAction=_,permissionClass=_,permissionDescripton=pdes}->
        pdes) rolperm
      )) (toList roles)
    ))) (toList uiTable)

--gives fixed length for str
fixStr :: String -> Int -> String
fixStr str n = setsp str (n - (length str))
        where setsp st i = if i<=0
                            then st
                            else setsp (st ++ " ") (i-1)

--gives number of indv perms
nmaxIPerm :: ([String], [(String, [String])]) -> Int
nmaxIPerm (iperm,_) = length iperm

--gives number og role perms
nmaxRPerm :: ([String], [(String, [String])]) -> Int
nmaxRPerm (_,rperm) = DL.foldl' (\c (_,perm) ->
        c+length perm
    ) 0 rperm

--displays in table form
showTable :: IO()
showTable = putStr (DL.foldl' (\ac x->DL.foldl' (\acstr(e,(r,rp),ip)-> acstr ++ (e++r++rp++ip)) ac x) "" (displayUITable (prepareUITable2)))


prepareUITable2 :: UITable
prepareUITable2 = DL.foldl' accumulateMap Map.empty joinResults
    where
        accumulateMap ::   UITable 
                        -> (Int,String,Maybe Int,Maybe String,Maybe String,Maybe String,Maybe Int,Maybe String,Maybe Int,Maybe String,Maybe String,Maybe String)
                        -> UITable
        accumulateMap tbl (uid, email, Just indPermId, Just indPermAction, Just indPermClass, Just indPermDesc, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) = 
            let key = User{userId=uid, userEmail=email}
                val = ( [Permission{permissionId = indPermId, permissionAction = indPermAction, permissionClass = indPermClass, permissionDescripton = indPermDesc}]
                      , Map.empty )
            in Map.insertWith mergeUser key val tbl
        accumulateMap tbl (uid, email, Nothing, Nothing, Nothing, Nothing, Just rid, Just rname, Just rPermId, Just rPermAction, Just rPermClass, Just rPermDesc) =
            let key = User{userId=uid, userEmail=email}
                val = ( []
                      , Map.singleton Role{roleId=rid, roleName=rname} [Permission{permissionId=rPermId, permissionAction=rPermAction, permissionClass=rPermClass, permissionDescripton=rPermDesc}]) 
            in Map.insertWith mergeUser key val tbl
        accumulateMap _ x = error $ "Invalid data received: " ++ (show x)

        mergeUser :: ([Permission], Map.Map Role [Permission]) -> ([Permission], Map.Map Role [Permission]) -> ([Permission], Map.Map Role [Permission])
        mergeUser (oldPerms, oldRoles) (newPerms, newRoles) = 
            let finalPerms = DL.union oldPerms newPerms
                finalRoles = Map.unionWith (\oldPerms_ newPerms_ -> DL.union oldPerms_ newPerms_) oldRoles newRoles
            in (finalPerms, finalRoles)