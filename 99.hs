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