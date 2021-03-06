import System.Random

-- @rickerbh's solutions to the Haskell 99 questions
-- http://www.haskell.org/haskellwiki/99_questions

-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs 

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "No item 1 from the end in an empty list"
myButLast [x] = error "No item 1 from the end in a singleton"
myButLast (x:xs)
    | length xs == 1 = x
    | otherwise = myButLast xs

-- Problem 3
elementAt :: (Num b, Ord b) => [a] -> b -> a
elementAt [] _ = error "No element"
elementAt (x:xs) n
    | n <= 0 = error "No element"
    | n == 1 = x
    | otherwise = elementAt xs (n-1)
    
-- Problem 4
myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
    | x == head (reverse xs) = isPalindrome (tail (reverse xs))
    | otherwise = False

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == head (xs) = compress xs
    | otherwise = [x] ++ compress xs
    
-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs)
    | elem x (head (pack xs)) = (x:(head (pack xs))):(tail (pack xs))
    | otherwise = [x]:(pack xs)

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = error "Can't encode an empty list"
encode xs = [(n, y) | (y:ys) <- (pack xs), let n = 1 + (length ys)]

-- Problem 11
data ListItem a = Single a | Multiple Int a
    deriving (Show)
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified [] = error "Can't encode an empty list"
encodeModified xs = [if n > 1 then (Multiple n x) else (Single x) | (n, x) <- encode xs]

-- Problem 12
decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = [x] ++ decodeModified xs
decodeModified ((Multiple n x):xs) = decodedMultiple n x ++ decodeModified xs
    where
      decodedMultiple 1 x = [x]
      decodedMultiple n x = [x] ++ decodedMultiple (n - 1) x

-- Problem 13
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = if x == head xs then countMultiples 2 xs else [Single x] ++ encodeDirect xs
    where
      countMultiples n (x:[]) = [Multiple n x]
      countMultiples n (x:xs) = if x == head xs then countMultiples (n + 1) xs else [Multiple n x] ++ encodeDirect xs
  
-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- Problem 15
repli :: (Num b, Ord b) => [a] -> b -> [a]
repli [] _ = []
repli _ 0 = []
repli [x] n = x:repli [x] (n - 1)
repli (x:xs) n = [x] ++ repli [x] (n - 1) ++ repli xs (n)

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n 1
    where
      dropEvery' [] _ _ = []
      dropEvery' (x:xs) n c
        | c `mod` n == 0 = dropEvery' xs n (c + 1)
        | otherwise = [x] ++ dropEvery' xs n (c + 1)

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = (firstPart xs n, secondPart xs n)
    where
        firstPart [] _ = []
        firstPart _ 0 = []
        firstPart (x:xs) n = [x] ++ firstPart xs (n - 1)
        secondPart [] _ = []
        secondPart xs 0 = xs
        secondPart (x:xs) n = secondPart xs (n - 1)

-- Problem 18
slice :: (Eq b, Num b) => [a] -> b -> b -> [a]
slice [] _ _ = []
slice _ 0 _ = error "List starts at 1"
slice _ 1 0 = []
slice (x:xs) 1 n = [x] ++ slice xs 1 (n - 1)
slice (x:xs) i n = slice xs (i - 1) (n - 1)

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n
  | n < 0 = rotate xs (length xs + n)
  | otherwise = reverse (take ((length xs) - n) (reverse xs)) ++ take n xs

-- Problem 20
removeAt :: Int -> [b] -> (b, [b])
removeAt n xs = (removed n xs, rest n xs)
    where
      removed 1 (x:xs) = x
      removed n (x:xs) = removed (n - 1) xs
      rest 1 (x:xs) = xs
      rest n (x:xs) = [x] ++ rest (n - 1) xs

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt y (x:xs) n
  | n == 1 = [y] ++ [x] ++ xs
  | otherwise = [x] ++ insertAt y xs (n - 1)

-- Problem 22
range :: Int -> Int -> [Int]
range x y
  | x > y = error "First operand must be less than or equal to the second"
  | x == y = [y]
  | otherwise = [x] ++ range (x + 1) y
  
-- Problem 23
--rnd_select :: [a] -> Int -> [a]
--rnd_select _ 0 = []
--rnd_select xs n = [(generateRandom xs)]
--    where
--      choose :: (Eq (IO Int)) => [a] -> IO Int -> a
--      choose (x:xs) n = if n == 1 then x else choose xs (n - 1)
--      generateRandom xs = randomRIO (1, length xs)

-- No idea how to use libraries. Have to come back to this

-- Problem 24
-- No idea how to use libraries. Have to come back to this

-- Problem 25
-- No idea how to use libraries. Have to come back to this

-- Problem 26

-- Problem 27

-- Problem 28
-- part 1 - implemented via quicksort - works, but doesn't retain original order
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = 
  let smallerSorted = lsort [a | a <- xs, length a <= length x]
      biggerSorted = lsort [a | a <- xs, length a > length x]
  in smallerSorted ++ [x] ++ biggerSorted

-- part 2

-- Problem 31
-- Not very efficient, but it'll do for now...
isPrime :: Int -> Bool
isPrime 1 = True
isPrime n = isPrime' n 2
    where
      isPrime' n c 
        | n == c = True
        | n `mod` c == 0 = False
        | otherwise = isPrime' n (c + 1)

-- Problem 32
myGCD :: Int -> Int -> Int
myGCD a b = if b == 0 then abs a else myGCD b (a `mod` b)

-- Problem 33
coprime :: Int -> Int -> Bool
coprime a b = myGCD a b == 1

-- Problem 34
totient :: Int -> Int
totient 1 = 1
totient n = sum [1 | x <- [1..(n - 1)], coprime n x]

-- Problem 35
primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2
    where
      primeFactors' n c
        | isPrime n = [n]
        | n == c = []
        | n `mod` c == 0 = [c] ++ primeFactors' (n `div` c) c
        | otherwise = primeFactors' n (nextPrime c)

nextPrime :: Int -> Int
nextPrime n = if isPrime (n + 1) then (n + 1) else nextPrime (n + 1)

-- Problem 36
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = [(x, n) | (n, x) <- encode (primeFactors n)]
  
-- Problem 37

-- Problem 38
-- no solution required

-- Problem 39
primesR :: Int -> Int -> [Int]
primesR x y = [n | n <- [x..y], isPrime n]

-- Problem 40
-- Inefficient, but it works
goldbach :: Int -> (Int, Int)
goldbach n = goldbach' n (primesR 2 n) (primesR 2 n)
    where
      goldbach' n xs ys
        | n `mod` 2 == 1 = error "Only even numbers sorry."
        | n < 4 = error "Number must be at least 4."
        | ys == [] = goldbach' n (tail xs) (primesR 2 n)
        | head xs + head ys == n = (head xs, head ys)
        | otherwise = goldbach' n xs (tail ys)

-- Problem 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList x y = [goldbach n | n <- [x..y], n `mod` 2 == 0]

goldbachList' x y n = [(a, b) | (a, b) <- goldbachList x y, a > n, b > n]

-- Problem 46
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

not' :: Bool -> Bool
not' True = False
not' _ = True

nand' :: Bool -> Bool -> Bool
nand' a b = not' (and' a b)

nor' :: Bool -> Bool -> Bool
nor' a b = not' (or' a b)

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

impl' :: Bool -> Bool -> Bool
impl' a b = (not' a) `or'` b

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

-- Problem 47

-- Problem 48

-- Problem 49
-- http://en.wikipedia.org/wiki/Gray_code
gray :: Int -> [[Char]]
gray 1 = ["0", "1"]
gray n = ["0" ++ n | n <- gray (n - 1)] ++ ["1" ++ n | n <- reverse (gray (n - 1))]

-- Problem 50

-- Problem 54A

-- Problem 55

-- Problem 56

-- Problem 57

-- Problem 58

-- Problem 59

-- Problem 60

-- Problem 95
fullWords :: Int -> String
fullWords n = fullWords' (show n)
    where 
      fullWords' (x:xs)
        | length (x:xs) == 1 = numberToString x
        | otherwise = numberToString x ++ "-" ++ fullWords' xs

numberToString :: Char -> String
numberToString c
    | c == '0' = "zero"
    | c == '1' = "one"
    | c == '2' = "two"
    | c == '3' = "three"
    | c == '4' = "four"
    | c == '5' = "five"
    | c == '6' = "six"
    | c == '7' = "seven"
    | c == '8' = "eight"
    | c == '9' = "nine"

-- Problem 96
isCharacterInString :: Char -> String -> Bool
isCharacterInString _ [] = False
isCharacterInString c (x:xs)
    | c == x = True
    | otherwise = isCharacterInString c xs

isUpperAlpha :: Char -> Bool
isUpperAlpha c = isCharacterInString c ['A'..'Z']

isLowerAlpha :: Char -> Bool
isLowerAlpha c = isCharacterInString c ['a'..'z']

isAlpha :: Char -> Bool
isAlpha c = isUpperAlpha c || isLowerAlpha c

isDigit :: Char -> Bool
isDigit c = isCharacterInString c ['0'..'9']

isHyphen :: Char -> Bool
isHyphen '-' = True
isHyphen _ = False

isNonHyphen :: Char -> Bool
isNonHyphen c = isAlpha c || isDigit c

identifier :: String -> Bool
identifier [] = False
identifier [x]
    | isAlpha x = True
    | otherwise = False
-- Need a special case for the first character to ensure it's a letter
identifier (x:xs)
    | isAlpha x && isHyphen (head xs) = identifier' (tail xs)
    | isAlpha x = identifier' xs
    | otherwise = False
identifier' [] = False
identifier' [x] = identifier [x]
identifier' (x:xs)
    | isHyphen x = False
    | isNonHyphen x && isNonHyphen (head xs) = identifier' xs
    | isNonHyphen x && isHyphen (head xs) = identifier' (tail xs)
