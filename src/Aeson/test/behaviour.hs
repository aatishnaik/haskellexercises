module Behavior where
import Text.Read
import Control.Monad.Trans.Maybe 
import Control.Monad.IO.Class

data Person = Person String Int Gender String
data Gender = Male | Female | NotSpecified deriving Read

instance Show Gender where
    show Male = "male"
    show Female = "female"
    show NotSpecified = "not specified"

instance Show Person where
    show (Person n a g j) = "Person {name: " ++ n ++ ", age: " ++ show a ++ ", gender: " ++ show g ++ ", job: " ++ j ++ "}"

askPerson :: IO (Maybe Person)
askPerson = do
  name <- putStr "Name? " >> getLine
  a <- putStr "Age? " >> getLine
  g <- putStr "Gender? " >> getLine
  job <- putStr "Job? " >> getLine
  return $ do age <- readMaybe a :: Maybe Int
              gender <- readMaybe g :: Maybe Gender
              --job <- readMaybe j :: Maybe String
              return $ Person name age gender job

askPersonT :: MaybeT IO Person
askPersonT = do
  name   <- liftIO $ putStr "Name? " >> getLine
  age    <- MaybeT $ fmap readMaybe $ putStr "Age? " >> getLine
  gender <- MaybeT $ fmap readMaybe $ putStr "Gender? " >> getLine
  job <- liftIO $ putStr "Job? " >> getLine
  --job <- MaybeT $ fmap readMaybe $ putStr "Job? " >> getLine
  return $ Person name age gender job

test1 = runMaybeT askPersonT