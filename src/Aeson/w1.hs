module Aeson.W where

import Text.Printf (printf)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy
import qualified Data.Map.Lazy as Map
import Text.Read (readMaybe)
import Prelude hiding (log)
import Control.Monad.Trans.Either

main = do
    e <- runEitherT $ do
        a <- lift getLine
        case a of
            "hi" -> lift $ putStrLn "hello!"
            _    -> left 1
        b <- lift getLine
        case b of
            "how are you?" -> lift $ putStrLn "fine, thanks!"
            _              -> left 2
        return "nice conversation"
    case e of
        Left  n   -> putStrLn $ "Error - Code: " ++ show n
        Right str -> putStrLn $ "Success - String: " ++ str