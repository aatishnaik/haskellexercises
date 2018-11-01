{-# LANGUAGE ScopedTypeVariables #-}
module Casava.Test where
import Data.List as DL
import Data.List.Split as DLS
import Text.Read as TR
import Data.Csv as DC
import Data.Text as DT
import Data.ByteString.Char8 as DB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

--mksheet :: Text
mksheet = DB.pack "aa,Geography,34\naa,Physics,13\naa,Economics,1334\nss,Geography,13\nss,Economics,1334"

main :: IO ()
main = do
    csvData <- BL.readFile "mk.csv"
    case decode NoHeader csvData of
        Left err -> Prelude.putStrLn err
        Right v -> V.forM_ v $ \ (name, salary :: Int) ->
            Prelude.putStrLn $ name ++ " earns " ++ show salary ++ " dollars"