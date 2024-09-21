# Laboratorio 1 - Haskell

## 3. Ejercicios sobre tipos básicos

**3.1 Escribir una función `siguienteLetra` que tome como argumento una letra del alfabeto y devuelva la letra que le sigue. Suponer que la letra ’A’ sigue a la ‘Z’ (tanto en mayu´sculas como en minúsculas).**

```haskell
siguienteLetra :: Char -> Char
siguienteLetra 'z' = 'a'
siguienteLetra 'Z' = 'A'
siguienteLetra c = chr . (+1) . ord $ c
```

**3.2 Escribir una función recursiva que devuelva el sumatorio desde un valor entero hasta otro.**

```haskell
summatory :: Int -> Int -> Int
summatory a b
    | a > b = summatory b a
    | a == b = a
    | otherwise = a + summatory (a + 1) b
```

**3.3 Escribir una función recursiva que devuelva el producto desde un valor entero hasta otro.**

```haskell
productsOf :: Int -> Int -> Int
productsOf a b
    | a > b = productsOf b a
    | a == b = a
    | otherwise = a * productsOf (a + 1) b
```

**3.4 Define una función binaria `maximo` que devuelve el mayor de sus dos argumentos.**

```haskell
maximo :: Int -> Int -> Int
maximo a b
    | a > b = a
    | otherwise = b
```

**3.5 Define una función `fact` para calcular el factorial de un número**

```haskell
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)
```
Otra opción si queremos reutilizar la función 'productsOf' previamente definida -> fact n = productsOf 1 n

**3.6 Usando la función anterior da una definición de la función `sumaFacts` tal que calcule la suma de los factoriales hasta un número n, es decir, `sumFacts n = fact 0 + fact 1 + . . . +
fact n`**

```haskell
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
```

## 5. Ejercicios a resolver sobre tipos listas

**5.1 Dada una lista de pares, define una función que devuelva la lista resultante de sumar cada par.**

```haskell
sumPairs :: [(Int, Int)] -> [Int]
sumPairs pairs = map sumPair pairs

sumPair :: (Int, Int) -> Int
sumPair (a, b) = a + b
```

**5.2 Define dos funciones que devuelvan el primer y último elemento de una lista.**

```haskell
-- Usando Maybe para gestionar el caso de las listas vacias
firstItem :: [Int] -> Maybe Int
firstItem [] = Nothing
firstItem list = Just $ head list

lastItem :: [Int] -> Maybe Int
lastItem [] = Nothing
lastItem list = Just $ last list
```

**5.3 Define una función para calcular la lista de divisores del número n. Utiliza listas intensionales.**

```haskell
calcDivisors :: Int -> [Int]
calcDivisors n = [divisor | divisor <- [1..n], n `mod` divisor == 0]
```

**5.4 Define una función para determinar si un entero pertenece a una lista de enteros.**

```haskell
contains :: Int -> [Int] -> Bool
contains _ [] = False
contains n (x:t)
    | n == x = True
    | otherwise = contains n t
```

**5.5 Define una función que reemplace en una lista todas las ocurrencias  de un elemento n por otro elemento p.**

```haskell
replace :: [Int] -> Int -> Int -> [Int]
replace [] _ _ = []
replace (x:t) a b
    | x == a = b : replace t a b
    | otherwise = x : replace t a b
```

**5.6 Define una función para contar cuantas veces aparece un elemento en una lista.**

```haskell
count :: [Int] -> Int -> Int
count [] _ = 0
count (x:t) n
    | x == n = 1 + count t n
    | otherwise = count t n
```

**5.7 Define una función que calcule los múltiplos de 5 (intenta dos versiones: la primera usando listas intensionales y la segunda usando orden superior)..**

```haskell
multiplesOf5 :: [Int]
multiplesOf5 = [x | x <- [1..], x `mod` 5 == 0]

multiplesOf5UsingFilter :: [Int]
multiplesOf5UsingFilter = filter (isMultipleOf5) [1..]
    where isMultipleOf5 x = x `mod` 5 == 0

-- Can be used with:
-- take 10 multiplesOf5
-- take 10 multiplesOf5UsingFilter
```

**5.8 Define una función que extraiga de una cadena los caracteres escritos en mayúscula.**

```haskell
extractCapitalLetters :: [Char] -> [Char]
extractCapitalLetters str = filter isCapitalLetter str
    where isCapitalLetter c = isUpper c
```