import Debug.Trace
import Data.List
temp = foldl' (\total x -> traceShow (total, x) (total + x)) 0 [1, 2, 3, 4, 5]

main :: IO()
main = print temp