module Aeson.W where

import Control.Monad.Trans.Class as TC
import Control.Monad
main :: IO()
main = do
  maybeUserName <- readUserName
  case maybeUserName of
    Nothing -> print "Invalid user name!"
    Just (uName) -> do
      maybeEmail <- readEmail
      case maybeEmail of
        Nothing -> print "Invalid email!"
        Just (email) -> do
          maybePassword <- readPassword
          case maybePassword of
            Nothing -> print "Invalid Password"
            Just password -> login uName email password

readUserName :: IO (Maybe String)
readUserName = do
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readEmail :: IO (Maybe String)
readEmail = do
    str <- getLine
    if length str > 5
      then return $ Just str
      else return Nothing

readPassword :: IO (Maybe String)
readPassword = do
    str <- getLine
    if length str > 5
      then return $ Just str
      else return Nothing

login :: String -> String -> String -> IO ()
login x y z = putStrLn (x++y++z)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
    return = lift . return
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)