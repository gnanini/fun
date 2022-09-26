module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code


--data ListInt = Empty | Cons Int ListInt
--    deriving (P.Show)
--
--instance P.Show ListInt where
--    show Empty          = "[]"
--    show (Cons x Empty) = "[" ++ (show'' x) ++ "]"
--    show (Cons x xs)    = "[" ++ (show'' x) ++ (show xs)
--        where
--            show'' :: Int -> Char
--            show'' x = show x
--
--    show x = "[" ++ show' x ++ "]"
--
--show' :: ListInt -> [Char]
--show' Empty          = "[]"
--show' (Cons x Empty) = show x
--show' (Cons x xs)    = show x ++ "," ++ show' xs



head :: [a] -> a
head (x:xs) = x

tail :: [a] -> [a]
tail (x:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length []     = 0
length (x:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum []        = 0
sum (x:xs)    = x + sum xs

product :: Num a => [a] -> a
product []     = error "Deu ruim"
product [x]    = x
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
(++) xs []     = xs
(++) [] ys     = ys
(++) (x:xs) ys = x : (++) xs ys

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc k []      = [k]
snoc k (x:xs)  = x : snoc k xs

(<:) :: [a] -> a -> [a]
(<:) xs x = snoc x xs

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum []          = error "vazio não tem mínimo"
minimum [x]         = x
minimum (x:y:[])
        | x < y     = x
        | x > y     = y
        | x == y    = x
minimum (x:y:xs)
        | x < y     = minimum (x:xs)
        | x > y     = minimum (y:xs)
        | x == y    = minimum (x:xs)


maximum :: Ord a => [a] -> a
maximum []          = error "vazio não tem máximo"
maximum [x]         = x
maximum (x:y:[])
        | x > y     = x
        | x < y     = y
        | x == y    = x
maximum (x:y:xs)
        | x > y     = maximum (x:xs)
        | x < y     = maximum (y:xs)
        | x == y    = maximum (x:xs)

-- take
take :: Int -> [a] -> [a]
take _ []     = error "Lista vazia"
take 0 _      = []
take 1 (y:ys) = [y]
take x (y:ys) = y : take (x-1) ys


-- drop
drop :: Int -> [a] -> [a]
drop 0 y      = y
drop k (x:xs) =  drop (k-1) xs


-- takeWhile
-- dropWhile

-- tails
tails :: [a] -> [[a]]
tails []     = [[]]
tails (x:xs) = (x:xs) : tails xs


-- init
init :: [a] -> [a]
init []     = error "dá certo não, abestado!!"
init [x]    = []
init (x:xs) = x : init xs


-- inits
inits :: [a] -> [[a]]
inits [x] = [[], [x]]
inits xs  = inits (init xs) ++ [xs] 


-- subsequences
subsequences :: [a] -> [[a]]
subsequences [x]    = inits [x]
subsequences (x:xs) = inits (x:xs) ++ subsequences xs


-- any --incompleta
any :: Foldable t => (a -> Bool) -> t a -> Bool --copiei do :type
any [] = False
any f (x:xs)


-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
--palindrome :: String -> Bool
--palindrome [] = True 
--palindrome x
--         | take (length x / 2) x == drop (length x / 2) = True
--         | otherwise                                    = False

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

