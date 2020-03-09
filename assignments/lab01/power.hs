
import Test.QuickCheck

-- | power function with product
power :: Integer -> Integer -> Integer
power n k = product $ replicate (fromInteger k) n

-- | power function with recursion O(log n)
power' :: Integer -> Integer -> Integer
power' n k | k == 0 = 1
           | even k = power' (n*n) (div k 2)
           | odd k  = n * power' n (k-1)


prop_power n k = power n k == power' n k