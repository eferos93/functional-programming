-- maxi x y returns the maximum of x and y
import Test.QuickCheck

maxi :: Ord a => a -> a -> a
maxi x y | x>=y      = x
         | otherwise = y


fib :: Integer -> Integer
fib n | n == 0 || n == 1 = 1
      | otherwise        = fib n-1 + fib n-2


nextFactor :: Integer -> Integer -> Integer
nextFactor k n | k == n           = n
               | mod n (k+1) == 0 = k+1
               | otherwise        = nextFactor (k+1) n

smallestFactor :: Integer -> Integer
smallestFactor n = nextFactor 1 n

propSmallestFact n = mod n (smallestFactor n) == 0

propSmallestFact' n = let m = smallestFactor n
                      in  n == (div n m) * m

allFactors n = [ factor | factor<-[1..n], mod n factor == 0]

prop_allFactors n = n>=1 ==> 1 `elem` fs && n `elem` fs
  where fs = allFactors n


multiply :: Num a => [a] -> a
multiply [] = 1
multiply (x:xs) = x * multiply xs

prop_multiply xs = multiply xs == product xs

duplicates :: Eq a => [a] -> Bool
duplicates []     = False
duplicates (x:xs) = elem x xs || duplicates xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []                         = []
removeDuplicates (x:xs) | elem x xs == True = removeDuplicates xs
                        | otherwise         = x:removeDuplicates xs

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))


data Month = M1 | M2 | M3 | M4 | M5 | M6 | M7 | M8 | M9 | M10 | M11 | M12
             deriving(Show,Eq,Ord,Read,Bounded,Enum)

daysInMonth :: Month -> Integer -> Integer
daysInMonth M2 year = if mod year 4 == 0 then 29 else 28
daysInMonth month _ | elem month [M4,M6,M9,M10] = 30
                    | otherwise                 = 31

data Date = Date { year::Integer, month::Month, day::Integer }
            deriving(Eq,Show,Ord,Read)

validDate :: Date -> Bool
validDate (Date y m d) = d >= 1 && d <= daysInMonth m y


tomorrow :: Date -> Date
tomorrow (Date y m d) | d < daysInMonth m y = Date y m (d+1)
                      | m < M12             = Date y (succ m) 1
                      | otherwise           = Date (y+1) M1 1