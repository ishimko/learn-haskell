{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Data.List
import qualified Data.Map as M

class Ord v => Vector v where
    distance :: v -> v -> Double

instance Vector (Double, Double) where
    distance (a, b) (c, d) = sqrt $ (c - a)**2 + (d - b)**2

class Vector v => Vectorizable e v where
    toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
    toVector = id

clusterAssignmentPhase :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
    let initialMap = M.fromList $ zip centroids (repeat [])
        in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p) (distance y $ toVector p) ) 
                                                         centroids
                          in M.adjust (p:) chosenCentroid m)
                 initialMap points
