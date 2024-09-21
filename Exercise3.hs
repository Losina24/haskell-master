module Exercises3 where

import Data.Char (toLower, ord, chr)

-- Exercise 3.1
siguienteLetra :: Char -> Char
siguienteLetra 'z' = 'a'
siguienteLetra 'Z' = 'A'
siguienteLetra c = chr . (+1) . ord $ c

-- Exercise 3.2
summatory :: Int -> Int -> Int
summatory a b
    | a > b = summatory b a
    | a == b = a
    | otherwise = a + summatory (a + 1) b

-- Exercise 3.3
productsOf :: Int -> Int -> Int
productsOf a b
    | a > b = productsOf b a
    | a == b = a
    | otherwise = a * productsOf (a + 1) b

-- Exercise 3.4
maximo :: Int -> Int -> Int
maximo a b
    | a > b = a
    | otherwise = b

-- Exercise 3.5
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)
-- Otra opción si queremos reutilizar la función 'productsOf' previamente definida -> fact n = productsOf 1 n

-- Exercise 3.6
sumaFacts :: Int -> Int
sumaFacts n = listSum $ getFactorials $ getAllNumbersBetween 0 n

getAllNumbersBetween :: Int -> Int -> [Int]
getAllNumbersBetween a b
    | a > b = []
    | a == b = [a]
    | otherwise = a : getAllNumbersBetween (a + 1) b

getFactorials :: [Int] -> [Int]
getFactorials [] = []
getFactorials (x:t) = fact x : getFactorials t

listSum :: [Int] -> Int
listSum [] = 0
listSum (x:t) = x + listSum t