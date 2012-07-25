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
-- Nested lists are not a native datatype - I'll have to learn how to define my own datatype before I tackle this.

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
-- I'll have to learn how to define my own datatype before I tackle this.

-- Problem 12
-- I'll have to learn how to define my own datatype before I tackle this.

-- Problem 13
-- I'll have to learn how to define my own datatype before I tackle this.

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

-- Problem 17
-- Could use splitAt but that would be cheating
--split xs n = splitAt n xs
-- Not really happy with this answer (I think "take" is considered a predefined predicate). 
-- A recursive version would be nice.
split :: [a] -> Int -> ([a], [a])
split xs n =
    let firstPart = take n xs
        secondPart = reverse (take ((length xs) - n) (reverse xs))
    in (firstPart, secondPart)

split' :: [a] -> Int -> ([a], [a])
split' xs 0 = ([], xs)

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

