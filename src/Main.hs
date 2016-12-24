module Main (main) where

import KMeans

example :: ([(Double, Double)], Int)
example = kMeans initializeSimple 2 ([(1, 1), (1,2), (4, 4), (4, 5)]::[(Double, Double)]) 0.001

showResult ::  ([(Double, Double)], Int) -> String
showResult (list, iterations) =  "Centroids: " ++ show list ++ " found with " ++ show iterations ++ " iterations"

main :: IO ()
main = putStrLn $ showResult example
