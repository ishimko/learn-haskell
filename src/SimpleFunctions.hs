module Chapter2 where
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