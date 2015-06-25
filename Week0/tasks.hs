import Data.Char (chr, ord)

--Task 1
even' :: Int -> Bool
even' x = x `mod` 2 == 0

--Task 2
odd' :: Int -> Bool
odd' x = not ( even' x )

--Task 3
bmi :: Double -> Double -> Double
bmi height weight = weight / ((height / 100) * (height / 100))

--Task 4
deg2Rad :: Double -> Double
deg2Rad deg = (deg / 180) * pi

--Task 5
rad2Deg :: Double -> Double
rad2Deg rad = (rad / pi) * 180

--Task 6
isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = (a + b) > c && (c + b) > a && (a + c) > b

--Task 7
perimeter :: Double -> Double -> Double -> Double
perimeter a b c = a + b + c

--Task 8
area :: Double -> Double -> Double -> Double
area a b c = sqrt ((p) * ((p) - a) * ((p) - b) * ((p) - c))
    where p = perimeter a b c / 2

--Task 9
calculate :: Char -> Double -> Double -> Double
calculate op x y =  if op == '+'
                    then x + y else
                    if op == '-'
                    then x - y else
                    if op == '*'
                    then x * y else
                    if op == '/'
                    then x / y else
                    error "Unknown operator!"

--Task 15
calculate' :: Char -> Double -> Double -> Double
calculate' '+' x y = x + y
calculate' '-' x y = x - y
calculate' '*' x y = x * y
calculate' '/' x y = x / y
calculate' lcur rcur num = error "Unknown operator!"

--Task 10
convert :: [Char] -> [Char] -> Double -> Double
convert lcur rcur num = if lcur == "usd" && rcur == "eur"
                    then num * 0.9 else
                    if lcur == "usd" && rcur == "bgn"
                    then num * 1.74 else
                    if lcur == "eur" && rcur == "usd"
                    then num * 1.12 else
                    if lcur == "eur" && rcur == "bgn"
                    then num * 1.96 else
                    if lcur == "bgn" && rcur == "eur"
                    then num * 0.51 else
                    if lcur == "bgn" && rcur == "usd"
                    then num * 0.57 else
                    error "Unknown operator!"

--Task 15
convert' :: [Char] -> [Char] -> Double -> Double
convert' "usd" "eur" num = num * 0.9
convert' "usd" "bgn" num = num * 1.74
convert' "eur" "usd" num = num * 1.12
convert' "eur" "bgn" num = num * 1.96
convert' "bgn" "eur" num = num * 0.51
convert' "bgn" "usd" num = num * 0.57
convert' lcur rcur num = error "Unknown operator!"

--Task 12
isTriangle' :: [Int] -> Bool
isTriangle' [a,b,c] = (a + b) > c && (c + b) > a && (a + c) > b

--Task 13
head' :: [a] -> a
head' (h:_) = h

--Task 14
tail' :: [a] -> [a]
tail' (_:xs) = xs

--Task 16
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

--Task 17
double :: [Int] -> [Int]
double [] = []
double (x:xs) = (x * 2) : double xs

--Task 18
mult :: Int -> [Int] -> [Int]
mult _ [] = []
mult y (x:xs) = (x * y) : mult y xs

--Task 19
nth :: Int -> [a] -> a
nth _ [] = error "Out of range"
nth 1 (x:xs) = x
nth y (x:xs)
    | y < 0 = error "Invalid index"
    | otherwise = nth (y-1) xs

--Task 20
member :: Int -> [Int] -> Bool
member y [] = False
member y (x:xs) = if y == x then True else member y xs

--for task 21
fib :: Int -> Int
fib n = helpFib 0 1 0
    where helpFib x y i
             | i == n = x
             | otherwise = helpFib y (x + y) (i + 1)

--task 21
isFibonacciSequence :: [Int] -> Bool
isFibonacciSequence [] = False
isFibonacciSequence [x] = x == fib 1
isFibonacciSequence x = iFSHelper x 0
    where iFSHelper [x] acc = x == fib acc
          iFSHelper (x:xs) acc
            | x == fib acc = iFSHelper xs (acc + 1)
            | otherwise = False

--Task 22
sum' :: Num a => [a] -> a
sum' (x:[]) = x
sum' [] = 0
sum' (x:xs) = x + sum' xs

--Task 23
product' :: [Int] -> Int
product' (x:[]) = x
product' [] = 0
product' (x:xs) = x * product' xs

--Task 24
multList :: [Int] -> [Int] -> [Int]
multList [] [] = []
multList (x:[]) (y:_) = [x * y]
multList (x:_) (y:[]) = [x * y]
multList (x:xs) (y:ys) = (x * y) : multList xs ys

--Task 25
number2string :: Int -> [Char]
number2string x = show x

--for task 25
figureInNumber :: Int -> Int
figureInNumber x
    | x < 10 = 1
    | otherwise = 1 + figureInNumber(x `div` 10)

--Task 25
number2string' :: Int -> [Char]
number2string' x
    | x < 10 = chr(x) : []
    | otherwise = chr(x `div` 10 ^ figureInNumber x) : number2string' (x `div` 10)

--Task 26
string2number' :: [Char] -> Int
string2number' x = read x

--Task 26
string2number :: [Char] -> Int
string2number [] = 0
string2number (x:[]) = ord x - ord '0'
string2number (x:xs) = (((ord x) - (ord '0')) * 10 ^ (length(x:xs) - 1)) + string2number xs

--task 27
--isValidID :: [Char] -> Int





--for task 28
fromIDtoDayAndMonth :: [Char] -> (Int, Int)
fromIDtoDayAndMonth month = ((string2number (month) `div` 10000) `mod` 100, (string2number (month) `div` 1000000) `mod` 100)

--for task 28 and 35
get1stInTuple :: (a, b) -> a
get1stInTuple (x, _) = x

--for task 28 and 35
get2ndInTuple :: (a, b) -> b
get2ndInTuple (_, x) = x

--for task 28
get1stTripleTuple :: ([Char], (Int, Int), (Int, Int)) -> [Char]
get1stTripleTuple (x, _, _) = x

--for task 28
get2ndTripleTuple :: ([Char], (Int, Int), (Int, Int)) -> (Int, Int)
get2ndTripleTuple (_, x, _) = x

--for task 28
get3thTripleTuple :: ([Char], (Int, Int), (Int, Int)) -> (Int, Int)
get3thTripleTuple (_, _, x) = x

--for task 28
whatZodiacSignIs' :: [Char] -> [([Char], (Int, Int), (Int, Int))] -> [Char]
whatZodiacSignIs' month (x:xs)
    | get2ndInTuple (fromIDtoDayAndMonth month) == get2ndInTuple(get2ndTripleTuple x) && get1stInTuple (fromIDtoDayAndMonth month) >= get1stInTuple(get2ndTripleTuple x) = get1stTripleTuple x
    | get2ndInTuple (fromIDtoDayAndMonth month) == get2ndInTuple(get3thTripleTuple x) && get1stInTuple (fromIDtoDayAndMonth month) <= get1stInTuple(get3thTripleTuple x) = get1stTripleTuple x
    | otherwise = whatZodiacSignIs' month xs

--task 28
whatZodiacSignIs :: [Char] -> [Char]
whatZodiacSignIs month = whatZodiacSignIs' month signs
    where signs = [
                ("Aries",      (21, 3),  (20, 4)),
                ("Taurus",     (21, 4),  (20, 5)),
                ("Gemini",  (21, 5),  (20, 6)),
                ("Cancer",       (21, 6),  (21, 7)),
                ("Leo",       (22, 7),  (22, 8)),
                ("Virgo",      (23, 8),  (22, 9)),
                ("Libra",     (23, 9),  (22, 10)),
                ("Scorpio",  (23, 10), (21, 11)),
                ("Sagittarius",   (22, 11), (21, 12)),
                ("Capricorn",   (22, 12), (19, 1)),
                ("Aquarius",   (20, 1),  (18, 2)),
                ("Pisces",      (19, 2),  (20, 3))
              ]

--for task 30
getLastElem :: [Int] -> Int
getLastElem [x] = x
getLastElem (x:xs) = getLastElem xs

--task 30
concatenate :: [a] -> [a] -> [a]
concatenate [] [] = []
concatenate [x] y = x : y
concatenate (x:xs) y = x : concatenate xs y

--task 31
init' :: [Int] -> [Int]
init' [] = error "You can't do that with the empty list!"
init' [x,y] = [x]
init' (x:xs) = x : (init' xs)

--task 32
take' :: Int -> [Int] -> [Int]
take' _ [] = []
take' x (y:ys)
    | x >= length (y:ys) = (y:ys)
    | x == 1 = [y]
    | x > 0 = y : take' (x-1) ys

--task 33
drop' :: Int -> [Int] -> [Int]
drop' _ [] = []
drop' x (y:ys)
    | x >= length (y:ys) = []
    | x == 1 = ys
    | x > 0 = drop' (x-1) ys

--task 34
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) [y] = [(x, y)]
zip' [x] (y:ys) = [(x, y)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

--for task 35
helpUnzip :: (a, b) -> ([a], [b]) -> ([a], [b])
helpUnzip (x,y) (z,t) = (x:z, y:t)

--task 35
unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([] , [])
unzip' [(x,y)] = ([x], [y])
unzip' (x:xs) = helpUnzip (get1stInTuple x, get2ndInTuple x) (unzip' xs)

--for task 36
isInList :: Int -> [Int] -> Bool
isInList x [y]
    | x == y = True
    |otherwise = False
isInList x (y:ys)
    | x == y = True
    |otherwise = isInList x ys

--for task 36
addToList :: Int -> [[Int]] -> [[Int]]
addToList _ [] = []
addToList x [y]
    | isInList x y = [x : y]
    | otherwise = [x] : [y]
addToList x (y:ys)
    | isInList x y = (x : y) : ys
    | otherwise =  y : addToList x ys

--task 36
group' :: [Int] -> [[Int]]
group' [] = []
group' [x] = [[x]]
group' (x:xs) = addToList x (group' xs)

--for task 37
isPyth :: Int -> Int -> Int -> Bool
isPyth x y z
    | ((x ^ 2) + (y ^ 2) == (z ^ 2)) || ((z ^ 2) + (y ^ 2) == (x ^ 2)) || ((x ^ 2) + (z ^ 2) == (y ^ 2)) = True
    | otherwise = False

--for task 37
isPythInList :: Int -> Int -> [Int] -> Bool
isPythInList _ _ [] = False
isPythInList x y [z]
    | isPyth x y z = True
    | otherwise = False
isPythInList x y (z:zs)
    | isPyth x y z = True
    | otherwise = isPythInList x y zs

--for task 37
getPythInList :: Int -> Int -> [Int] -> (Int, Int, Int)
getPythInList x y (z:zs)
    | isPyth x y z = getPyth x y z
    | otherwise = getPythInList x y zs

--for task 37
getPyth :: Int -> Int -> Int -> (Int, Int, Int)
getPyth x y z
    | z > y && y >= x = (x ,y ,z)
    | x > y && y >= z = (z, y, x)
    | z > x && x >= y = (y, x, z)
    | x > z && z >= y = (y, z, x)
    | y > x && x >= z = (z, x, y)
    | otherwise = (x, z, y)

--for task 37
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

--for task 37
helpPyths :: Int -> [Int] -> [Int] -> [(Int, Int, Int)]
helpPyths _ [] [] = []
helpPyths x y [z]
    | isPythInList x z y = [getPythInList x z y]
    | otherwise = []
helpPyths x y (z:zs)
    | isPythInList x z y = getPythInList x z y : helpPyths x y zs
    | otherwise = helpPyths x y zs

--for task 37
helpPyths' :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
helpPyths' [] _ _ = []
helpPyths' [x] y z = helpPyths x y z
helpPyths' (x:xs) y z
    | (helpPyths x y z) == [] = (helpPyths' xs y z)
    | otherwise = concatenate (helpPyths x y z) (helpPyths' xs y z)

--task 37
pyths :: Int -> Int -> [(Int, Int, Int)]
pyths x y = removeDuplicates (helpPyths' [x..y] [x..y] [x..y])

--task 38
multiplyBy :: Int -> (Int -> Int)
multiplyBy x = \y -> y * x 

--for task 39
getDigitsInNumber :: Int -> [Int]
getDigitsInNumber x
    | x < 10 = [x]
    | otherwise = (x `mod` 10) : getDigitsInNumber(x `div` 10)

--taks 39
lastDigits :: [Int] -> [Int]
lastDigits [] = []
lastDigits [x] = getDigitsInNumber x
lastDigits (x:xs) = removeDuplicates (concatenate (getDigitsInNumber x) (lastDigits xs))

--task 40
stringsToIntegers :: [[Char]] -> [Int]
stringsToIntegers [] = []
stringsToIntegers [x] = [string2number x]
stringsToIntegers (x:xs) = string2number x : stringsToIntegers xs

--task 41
fibonaccis :: [Int] -> [Int]
fibonaccis [] = []
fibonaccis [x] = [fib x]
fibonaccis (x:xs) = fib x : fibonaccis xs

--task 42
--applyToAll :: (Int -> Int) -> ([Int] -> [Int])




--task 43 for rewriting





--task 44
odds :: [Int] -> [Int]
odds [] = []
odds [x]
    | odd' x = [x]
    | otherwise = []
odds (x:xs)
    | odd' x = x : odds xs
    | otherwise = odds xs

--task 45
divisibles :: Int -> ([Int] -> [Int])
divisibles x = \y -> dHelper y
    where dHelper [] = []
          dHelper [y]
              | y `mod` x == 0 = [y]
              | otherwise = []
          dHelper (y:ys)
              | y `mod` x == 0 = y : dHelper ys
              | otherwise = dHelper ys

--task 46
filterBy :: (Int -> Bool) -> ([Int] -> [Int])
filterBy func = \x -> helpFunc x
    where helpFunc [] = []
          helpFunc [x]
              | func x = [x]
              | otherwise = []
          helpFunc (x:xs)
              | func x = x : helpFunc xs
              | otherwise = helpFunc xs

--task 47 for rewriting



--task 48
concat' :: [[Int]] -> [Int]
concat' [] = []
concat' [x] = x
concat' (x:xs) = concatenate x (concat' xs)

--task 49
reduce :: (Int -> Int -> Int) -> Int -> ([Int] -> Int)
reduce func x = \y -> summAllInList func x y
    where summAllInList _ _ [] = 0
          summAllInList func x [y] = func y x
          summAllInList func x (y:ys) = func (func y x) (summAllInList func x ys)

--task 50

--task 51
