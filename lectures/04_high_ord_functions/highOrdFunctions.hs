
import Prelude hiding (map,filter,sum,product,concat,foldr,
                       takeWhile,dropWhile,lines)
import Data.Char(isSpace)
import Data.List(sort,group)

-- 

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x:map f xs


filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) | p x       = x:filter p xs
                | otherwise = filter p xs

filter' p xs = foldr keep [] xs
  where keep x ys | p x       = x:ys
                  | otherwise = ys

-- * Some examples of first order functions on lists

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

-- * Factor out the differences from the common pattern

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op u []     = u
foldr op u (x:xs) = x `op` foldr op u xs


sum' xs = foldr (+) 0 xs
product' xs = foldr (*) 1 xs
concat' xss = foldr (++) [] xss


-- * More examples

weather = "June is warm\nJuly is warm\nJanuary is cold\n"

takeLine "" = ""
takeLine (c:cs) | c/='\n' = c:takeLine cs
                | otherwise = ""

takeWord "" = ""
takeWord (c:cs) | not (isSpace c) = c:takeWord cs
                | otherwise       = ""


takeWhile p [] = []
takeWhile p (x:xs) | p x       = x:takeWhile p xs
                   | otherwise = []




--------------------------------------------------------------------------------
-- * For ways to create functions without giving them a name
-- This is useful when we pass functions as arguments to higher order functions

takeLine_ s = takeWhile notNl s
  where notNl c = c/='\n' -- giving the function a name

takeLine' s = takeWhile (/='\n') s   -- sections

takeWord' s = takeWhile (not . isSpace) s -- function composition

takeWord'' s = takeWhile (\c->not (isSpace c)) s -- lambda abstraction

-- f x = ...x....x...
-- f = \ x -> ...x...x...

notEqual x y = x/=y

takeWord''' s = takeWhile (notEqual '\n') s  -- partial application

--------------------------------------------------------------------------------
-- * Functions returning functions (example from the slides)

pick :: Int -> (a,a) -> a
pick 1 = fst
pick 2 = snd

--------------------------------------------------------------------------------
-- * A larger example: counting words in a string
-- and produce nicely formatted output, most common word first,
-- written in "point-free style"

wordCounts :: String -> String
wordCounts = unlines .
             map (\(n,w)->w++": "++show n) .
             reverse .
             sort .
             map (\ws->(length ws,head ws)) .
             group .
             sort .
             words

--------------------------------------------------------------------------------
-- * Filled in after the lecture

dropWhile :: (a->Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) | p x       = dropWhile p xs
                   | otherwise = x:xs

dropLine s = dropWhile (/='\n') s

-- | Break a string into lines (first order)
lines :: String -> [String]
lines "" = []
lines s = takeLine s : lines (drop 1 (dropLine s))

-- | Generalize lines into segments (higher order)
segments :: (a->Bool) -> [a] -> [[a]]
segments p [] = []
segments p xs = takeWhile p xs : segments p (drop 1 (dropWhile p xs))

f3 xs = foldr snoc [] xs
    where snoc x ys = ys++[x]