module Chapter2 where
    firstOrEmpty :: [[Char]] -> [Char]
    firstOrEmpty lst = if not (null lst) then head lst else "empty"

    (+++) :: [t] -> [t] -> [t]
    lst1 +++ lst2 = if null lst1
                    then lst2
                    else head lst1 : (tail lst1 +++ lst2)

    reverse2 :: [t] -> [t]
    reverse2 lst = if null lst
                   then []
                   else reverse (tail lst) +++ [head lst]

    maxmin :: (Ord t) => [t] -> (t, t)
    maxmin list = let h = head list
                  in if null (tail list)
                     then (h, h)
                     else ( if h > t_max then h else t_max
                          , if h < t_min then h else t_min )
                          where t = maxmin (tail list)
                                t_max = fst t
                                t_min = snd t

    fibonacci :: Integer -> Integer
    fibonacci n = case n of
                    0 -> 0
                    1 -> 1
                    _ -> fibonacci (n-1) + fibonacci (n-2)