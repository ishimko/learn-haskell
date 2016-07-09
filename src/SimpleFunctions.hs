module SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [t] -> [t] -> [t]
[] +++ list2 = list2
(x:xs) +++ list2 = x:(xs +++ list2)

reverse2 :: [t] -> [t]
reverse2 [] = []
reverse2 (x:xs) = reverse2 xs ++ [x]

maxmin :: (Ord t) => [t] -> (t, t)
maxmin [x] = (x, x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min
                ) where (xs_max, xs_min) = maxmin xs

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

qsort :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) =  qsort lesser ++ [x] ++ qsort greater
                   where   lesser = [a | a <- xs, a <= x]
                           greater = [a | a <- xs, a > x]

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort list = current ++ greater
                        where   current = [x | x <- list, x == first]
                                greater = selectionSort [x | x <- list, x > first]
                                first = minimum list

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' (x:xs) = ( fst x : fst unzipped
                , snd x : snd unzipped
                ) where unzipped = unzip' xs

filterOnes :: [Integer] -> [Integer]
filterOnes = filter (== 1)

filterAtElement :: (Eq a) => a -> [a] -> [a]
filterAtElement element = filter (== element)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

product' :: (Num a, Foldable b) => b a -> a
product' = foldr (*) 1

product'' :: (Num a) => [a] -> a
product'' [] = 1
product'' (x:xs) = x * product'' xs

all' :: Foldable a => a Bool -> Bool
all' = foldr (&&) True

all'' :: [Bool] -> Bool
all'' [] = True
all'' (x:xs) = x && all'' xs

minimumBy :: (Ord a, Foldable b) => (c -> a) -> b c -> c
minimumBy f = foldr1 (\x y -> if f x < f y then x else y)