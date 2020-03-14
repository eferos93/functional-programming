-- maxi x y returns the maximum of x and y

maxi :: Ord a => a -> a -> a
maxi x y | x>=y      = x
         | otherwise = y


fib n :: Integer -> Integer
fib n | n == 0 || n == 1 = 1
      | otherwise = fib n-1 + fib n-2