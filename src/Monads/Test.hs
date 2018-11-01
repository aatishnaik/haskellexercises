module Monad.Test where
import Data.List
data PaxType = Adult | Child | Infant
data ValidatedAge = ValidatedAge PaxType Int

validateAgeAndPaxType :: Maybe Int -> Maybe PaxType -> Maybe ValidatedAge
validateAgeAndPaxType mAge mPaxType = case mPaxType of
  Just Adult -> case mAge of
    Nothing -> Nothing
    Just age -> if age>=18 then (Just $ ValidatedAge Adult age) else Nothing
  Just Child -> case mAge of
    Nothing -> Nothing
    Just age -> if age<18 && age>=5 then (Just $ ValidatedAge Child age) else Nothing
  Just Infant -> case mAge of
    Nothing -> Nothing
    Just age -> if age<5 then (Just $ ValidatedAge Infant age) else Nothing
  Nothing -> Nothing

readVals :: IO [Int]
readVals = fmap (map read.words) getLine