import Prelude hiding ((++),null,length,sum,reverse,take,drop,splitAt,zip,unzip)
import qualified Prelude

import Test.QuickCheck
--------------------------------------------------------------------------------

null :: [a] -> Bool
null [] = True
null _  = False


length :: [a] -> Int
length [] = 0
length (x:xs) = length xs + 1

sum :: Num a => [a] -> a
sum [] = 0
sum (n:ns) = n + sum ns

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs++ys)


cons :: a -> [a] -> [a]
cons x xs = x:xs         -- O(1)

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]    -- O(n)

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]  -- O(n^2)

-- complexity? how to make it more efficient?

-- | Take the first n elements of a list
take :: Int -> [a] -> [a]
take 0 xs = []
take _ [] = []
take n (x:xs) = x:take (n-1) xs


prop_take n xs = length (take n' xs) == min n' (length xs)
  where
    n' = abs n

-- | Discard the first n elements of a list
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

prop_take_drop :: Int -> [Int] -> Bool
prop_take_drop n xs = take n xs ++ drop n xs == xs

nonprop_take_drop :: Int -> [Int] -> Bool
nonprop_take_drop n xs = drop n xs ++ take n xs == xs


-- | "Quicksort"
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger
  where
    smaller = [y | y<-xs, y<=x]
    bigger  = [y | y<-xs, y>x]

-- | insert a new element at the right position in a sorted list
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (x':xs) | x<x'      = x:x':xs
                 | otherwise = x':insert x xs

isSorted [] = True
isSorted [x] = True
isSorted (x:x':xs) = x<=x' && isSorted (x':xs)

prop_insert :: Int -> [Int] -> Property
prop_insert x xs = isSorted xs ==> isSorted (insert x xs)
-- quickCheck says "Gave up!" because most of the random lists it generates
-- are not sorted. We will return to this issue in a future lecture.

-- | Insertion sort (sort a list by using insert)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

prop_qsort :: [Int] -> Bool
prop_qsort xs = qsort xs == isort xs

--------------------------------------------------------------------------------
-- * Filled in after the lecture


-- | splitAt n xs = (take n xs,drop n xs)
splitAt :: Int -> [a] -> ([a],[a])
splitAt 0 xs = ([],xs)
splitAt n [] = ([],[])
splitAt n (x:xs) = (x:ys,zs)
  where
    (ys,zs) = splitAt (n-1) xs
    -- When calling a function that returns a pair, it is often useful to use
    -- pattern matching to access the two parts of the pair

-- | Combine a pair of list into a list of pairs
zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y):zip xs ys
zip _      _      = []
-- We have two lists that both can be empty or non-empty, so there are
-- 4 combinations to consider. By putting the case when both lists are
-- non-empty first, all the other cases (one or both lists are empty) can
-- be handled with one equation (since zip stops and returns the empty list
-- when it reaches the end of the shorter of the two lists).

prop_length_zip xs ys = length (zip xs ys) == min (length xs) (length ys)

-- | Split a list of pairs into a pair of lists
unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip ((x,y):xys) = (x:xs,y:ys)
  where
    (xs,ys) = unzip xys
    
prop_zip_unzip :: [(Int,Int)] -> Bool
prop_zip_unzip xys = zip xs ys == xys
  where
    (xs,ys) = unzip xys

prop_unzip_zip :: [Int] -> [Int] -> Bool
prop_unzip_zip xs ys = unzip (zip xs ys) == (take n xs,take n ys)
  where
    n = min (length xs) (length ys)