{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Typeclass.Tst where
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
joinResults = [(1,"abc@abc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(1,"abc@abc.com",Just 1,Just "Reserve Bus",Just "Trips::Trip",Just "Allowed to edit Bus",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(2,"bbc@bc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(1,"abc@abc.com",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(3,"ccc@abc.com",Just 1,Just "Reserve Bus",Just "Trips::Trip",Just "Allowed to edit Bus",Nothing,Nothing,Nothing,Nothing,Nothing,Nothing),(3,"ccc@abc.com",Nothing,Nothing,Nothing,Nothing,Just 1,Just "Reservation Manager",Just 1,Just "manage_calendar",Just "Trips::Trip",Just "Allowed to edit Departure calendar"),(3,"ccc@abc.com",Nothing,Nothing,Nothing,Nothing,Just 2,Just "Reservation Manager",Just 2,Just "manage_Bus",Just "Trips::Trip",Just "Allowed to edit Bus")]

prepareUITable :: UITable
prepareUITable = DL.foldl' accumulateMap Map.empty joinResults
    where
        accumulateMap ::    UITable 
                        -> (Int,String,Maybe Int,Maybe String,Maybe String,Maybe String,Maybe Int,Maybe String,Maybe Int,Maybe String,Maybe String,Maybe String) -> 
            accMap (usrId,usrEmail,ipermId,ipermAct,ipermCls,ipermDes,rolId,rolName,fpermId,fpermAct,fpermCls,fpermDes)->
                case (ipermId,ipermAct,ipermCls,ipermDes) of
                    (Just pid,Just pact,Just pcls,Just pdes) -> 
                        insertWith () (User {userId=usrId,userEmail=usrEmail}) (
                            ([Permission {permissionId=ipermId,permissionAction=ipermAct,permissionClass=ipermCls,permissionDescripton=ipermDes}]),
                            (Map.empty)
                        ) accMap
                    (Nothing,Nothing,Nothing,Nothing) -> 
                        insertWith () (User {userId=usrId,userEmail=usrEmail}) (
                            (_iperms,
                            _roles
                        ) accMap
            