{-# LANGUAGE FlexibleInstances,GeneralizedNewtypeDeriving,MultiParamTypeClasses,TypeFamilies,UndecidableInstances #-}
{-module TestTweet where
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.Reader

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."
countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntriesTrad newName
              else return []
  return $ (path, length contents) : concat rest
countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName
newtype MyApp a = MyA {
  runA :: ReaderT AppConfig (StateT AppState IO) a
} deriving (Monad, MonadIO, MonadReader AppConfig,MonadState AppState)
runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT (runA k) config) state-}
{-module MaybeTParse
    (Parse,evalParse) where

import MaybeT
import Control.Monad.State
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L
data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

newtype Parse a = P {
      runP :: MaybeT (State ParseState) a
    } deriving (Monad, MonadState ParseState)
evalParse :: Parse a -> L.ByteString -> Maybe a
evalParse m s = evalState (runMaybeT (runP m)) (ParseState s 0)-}

{-module TestTweet where
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

greet :: IO ()
greet = do putStr "enter name: "
           n <- getLine
           putStrLn $ "Hello " ++ n 

mgreet::Reader IO ()
mgreet = 
  do 
    liftIO $ putStr "enter name: "
    n <- liftIO getLine
    liftIO $ putStrLn $ "Hello " ++ n-}


{-import Control.Monad.Trans.Reader
myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)-}

{-import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)

type App a = ReaderT AppConfig (StateT AppState IO) a
runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT k config) state


constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfgMaxDepth cfg
              then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $
                  put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
              else return []
  pure $ (path, length contents) : concat rest

newtype MyApp a = MyA {
    runA :: ReaderT AppConfig (StateT AppState IO) a
} deriving (Monad, MonadIO, MonadReader AppConfig,MonadState AppState)

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in runStateT (runReaderT (runA k) config) state-}

--runMyApp (MyA $ constrainedCount 0 ".") 4
{-module Test where
import Control.Monad.Readertype Config = FilePath

load :: (MonadReader Config m, MonadIO m) => String -> m String
load x = do
    config <- ask
    liftIO $ readFile (config ++ x)

loadRevision :: (MonadReader Config m, MonadIO m) => Int -> m String
loadRevision x = load ("history" ++ show x ++ ".txt")

loadAll :: (MonadReader Config m, MonadIO m) => Int -> String -> m (String, String)
loadAll x y = do
    a <- load y
    b <- loadRevision x
    return (a, b)
-}
import Control.Monad.Reader
import qualified Data.Map.Lazy as M

type Config = M.Map String String
tom :: Reader String String
tom = do
    env <- ask -- gives you the environment which in this case is a String
    return (env ++ " This is Tom.")

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
    t <- tom
    j <- jerry
    return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = (runReader tomAndJerry) "Who is this?"


