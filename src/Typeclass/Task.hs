module Typeclass.Task where
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

-- [(userId, email)]
userList :: [(Int, String)]
userList = [(1,"abc@abc.com"),(2,"bbc@abc.com"),(3,"cbc@cbc.com"),(4,"dbc@abc.com")]

-- [(userId, permissionId, action, class, description)]
individualPermissions :: [(Int, Int, String, String, String)]
individualPermissions = [(1,3,"manage_calendar ","Trips::Trip","Allowed to edit Departure calendar"),(3,1,"manage_agents","Common::Client","Allowed to manage agent clients"),(4,4,"override_capacity_constraints","Itinerary::Booking  ","Allowed to override booking capacity constraints")]

-- [(userId, roleId, roleName)]
roleList :: [(Int, Int, String)]
roleList = [(1,1,"Basic User"),(2,5,"Reservation Manager"),(3,4,"Reservation Staff")]

-- [(userId, roleId, permissionId)]
rolePermissions :: [(Int,Maybe Int,Maybe Int)]
rolePermissions = [(1,Just 3,Nothing),(1,Nothing,Just 4)]

-- [(userId, roleId, permissionId, action, class, description)]
permissionList :: [(Int,Int,Int, String, String, String)]
permissionList = [(1,1,1,"manage_agents","Common::Client","Allowed to manage agent clients"),(1,2,1,"manage_agents","Common::Client","Allowed to manage agent clients"),(2,1,1,"manage_agents","Common::Client","Allowed to manage agent clients"),(3,1,3,"manage_calendar","Trips::Trip","Allowed to edit Departure calendar"),(4,1,3,"manage_calendar","Trips::Trip","Allowed to edit Departure calendar")]

--prepareUITable :: UITable
prepareUITable = DL.map (\(uluId,ulEmail)->(
  --ulEmail,
    let 
      rset = DL.filter (\(pluId,_,_,_,_,_)-> pluId == uluId) permissionList
      rList = DL.map (\(_,trid,_,_,_,_)-> DL.foldl' (\arr (rluId,rlrolId,rlDes)-> if (rluId==uluId && rlrolId==trid) then arr++[] ) [] roleList)) rset
      --permset = DL.nub (DL.map (\(pluId,plrolId,plpermId,_,_,plDes)->(pluId,plrolId,plpermId,plDes)) permissionList)
      rolperm = DL.nub (DL.map (\(pluId,plrolId,plpermId)->(pluId,plrolId,plpermId)) (DL.filter (\(rluId,_,_)->rluId==uluId) roleList))
    in rolperm--DL.map (\(rlId,rlName)-> (rlName,DL.map (\(prolid,pid,pdesc)-> pdesc) (DL.filter (\(idrol,idp,descp)-> rlId==idrol) permset))) rList
  --,DL.map (\(_,_,_,_,e)->e) (DL.filter (\(ipuId,_,_,_,_)-> ipuId == uluId) individualPermissions)
  )) userList

--DL.map (\(usrid,rolid,rolname) -> (DL.filter (\(uid,rid,pid,ac,cl,des) -> rid == rolid) permissionList)) roleList

-- This function will generate a well-formatted (mulit-line) string to display
-- the UI table on the console.
--displayUITable :: UITable -> String
--displayUITable = _writeThisFunction