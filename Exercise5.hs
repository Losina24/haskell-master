module Exercises5 where

import Data.Char (isUpper)

-- Exercise 5.1
sumPairs :: [(Int, Int)] -> [Int]
sumPairs pairs = map sumPair pairs

sumPair :: (Int, Int) -> Int
sumPair (a, b) = a + b

-- Exercise 5.2
-- Usando Maybe para gestionar el caso de las listas vacias
firstItem :: [Int] -> Maybe Int
firstItem [] = Nothing
firstItem list = Just $ head list

lastItem :: [Int] -> Maybe Int
lastItem [] = Nothing
lastItem list = Just $ last list

-- Exercise 5.3
calcDivisors :: Int -> [Int]
calcDivisors n = [divisor | divisor <- [1..n], n `mod` divisor == 0]

-- Exercise 5.4
contains :: Int -> [Int] -> Bool
contains _ [] = False
contains n (x:t)
    | n == x = True
    | otherwise = contains n t

-- Exercise 5.5
replace :: [Int] -> Int -> Int -> [Int]
replace [] _ _ = []
replace (x:t) a b
    | x == a = b : replace t a b
    | otherwise = x : replace t a b

-- Exercise 5.6
count :: [Int] -> Int -> Int
count [] _ = 0
count (x:t) n
    | x == n = 1 + count t n
    | otherwise = count t n

-- Exercise 5.7 (listas intensionales)
multiplesOf5 :: [Int]
multiplesOf5 = [x | x <- [1..], x `mod` 5 == 0]

multiplesOf5UsingFilter :: [Int]
multiplesOf5UsingFilter = filter (isMultipleOf5) [1..]
    where isMultipleOf5 x = x `mod` 5 == 0

-- Can be used with:
-- take 10 multiplesOf5
-- take 10 multiplesOf5UsingFilter

-- Exercise 5.8
extractCapitalLetters :: [Char] -> [Char]
extractCapitalLetters str = filter isCapitalLetter str
    where isCapitalLetter c = isUpper c