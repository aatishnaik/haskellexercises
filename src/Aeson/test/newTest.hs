module NewTest where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad
import System.FilePath
type Config = FilePath

load :: (MonadReader Config m, MonadIO m) => String -> m String
load x = do
    config <- ask
    liftIO $ readFile (config ++ x)

loadRevision :: Int -> m String
loadRevision x = load ("history" ++ show x ++ ".txt")

loadAll :: Int -> String -> m (String, String)
loadAll x y = do
    a <- load y
    b <- loadRevision x
    return (a, b)