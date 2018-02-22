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

type UITable = Map.Map User ([Permission], [Map Role [Permission]])
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

prepareUITable :: Map.Map User ([Permission], [Map Role [Permission]])
prepareUITable = Map.fromList --outer map
  (DL.map (\(uluId,ulEmail)->(
    --attach user object
    User {userId=uluId,userEmail=ulEmail},( 
      --attach permission list from indv permissions of the userid
      DL.map (\(_,ipPid,ipAct,ipCls,ipDes)-> Permission {permissionId=ipPid,permissionAction=ipAct,permissionClass=ipCls,permissionDescripton=ipDes}
      ) (DL.filter (\(ipUId,_,_,_,_)-> uluId == ipUId) individualPermissions) --filter indv permissions mattching the user id
      ,
      --iterate through rolelist
      DL.map
        (\(_,rpRolId,_)->
          -- inner Map
          Map.fromList (
              --iterate through roles having a value
              DL.foldl' (\arr (rlUId,rlRolId,rlName) ->
                --attach role object
                arr ++ [(Role {roleId=rlUId,roleName=rlName},
                      --map to iterate and attach permission object
                      DL.map (\(_,_,plPermId,plAct,plCls,plDes)-> Permission{permissionId=plPermId,permissionAction=plAct,permissionClass=plCls,permissionDescripton=plDes}) (DL.filter (\(plUId,plRolId,_,_,_,_)-> plUId == uluId && plRolId == rlRolId) permissionList))]
              ) [] (DL.filter (\(rlUId,_,_) -> rlUId == uluId && 
                (case rpRolId of --filter roles and choose that have a value
                  Just _ -> True
                  Nothing -> False)
              ) roleList)
            )
        )
      rolePermissions
  ))) userList)

--displayUITable :: UITable -> 
{--displayUITable uiTable = Map.map (\(k,v)->
    let (indv,rol) = v
    in Map.map () rol
  ) uiTable--}
