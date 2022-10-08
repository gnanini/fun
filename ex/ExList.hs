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
length (_:xs) = 1 + length xs


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


take :: Int -> [a] -> [a]
take _ []     = error "Lista vazia"
take 0 _      = []
take x (y:ys) = y : take (x-1) ys


drop :: Int -> [a] -> [a]
drop _ []     = [] --error "Lista vazia"
drop 0 xs      = xs
drop k (x:xs) =  drop (k-1) xs


takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []             = []
takeWhile f (x:xs)
            | f x == True  = x : takeWhile f xs
            | otherwise    = []


-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x:xs)
        | f x == False = x:xs
        | otherwise = dropWhile f xs


tails :: [a] -> [[a]]
tails []     = [[]]
tails (x:xs) = (x:xs) : tails xs


init :: [a] -> [a]
init []     = error "dá certo não, abestado!!"
init [x]    = []
init (x:xs) = x : init xs


inits :: [a] -> [[a]]
inits [x] = [[], [x]]
inits xs  = inits (init xs) ++ [xs] 


subsequences :: [a] -> [[a]]
subsequences [x]    = inits [x]
subsequences (x:xs) = inits (x:xs) ++ subsequences xs


any :: (a -> Bool) -> [a] -> Bool
any f []               = False
any f [x]              = f x
any f (x:xs)
        | f x == True  = True 
        | otherwise    = any f xs


all :: (a -> Bool) -> [a] -> Bool
all f []               = True
all f [x]              = f x
all f (x:xs)
        | f x == False = False 
        | otherwise    = all f xs


and :: Bool -> Bool -> Bool
and True True = True
and _ _       = False


or :: Bool -> Bool -> Bool
or False False = False
or _ _         = True


concat :: [[a]] -> [a]
concat [[]]   = []
concat [x]    = x
concat (x:xs) = x ++ (concat xs)


elem :: (Eq a) => a -> [a] -> Bool  
elem x xs = any (== x) xs


-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: (Eq a) => a -> [a] -> Bool  
elem' _ []           = False
elem' x (y:ys)
        | x == y    = True
        | otherwise = elem' x ys


(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) []     _ = error "indíce grande demais, abestalhado!!"
(!!) (x:xs) n = (!!) xs (n-1)


filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs)
        | f x == True = x : filter f xs
        | otherwise   = filter f xs


map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs


cycle :: [a] -> [a]
cycle []     = error "lista vazia, caralhos!!"
cycle [x]    = x : cycle [x]
cycle (x:xs) = x:xs ++ cycle (x:xs)


repeat :: a -> [a]
repeat x = x : repeat x


replicate :: Int -> a -> [a]
replicate 0 x  = []
replicate n x  = x : replicate (n-1) x


isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _              = True
isPrefixOf (x:xs) (y:ys)
                 | x == y    = isPrefixOf xs ys
                 | otherwise = False


isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _            = True
isInfixOf _ []            = False
isInfixOf (x:xs) (y:ys)
    | checa (x:xs) (y:ys) = True
    | otherwise           = isInfixOf (x:xs) ys
    where
        checa :: Eq a => [a] -> [a] -> Bool
        checa [] _              = True
        checa _ []              = False
        checa (x:xs) (y:ys)
                    | x == y    = checa xs ys
                    | otherwise = False


isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _           = True
isSuffixOf _ []           = False
isSuffixOf (x:xs) (y:ys)
    | checa (x:xs) (y:ys) = True
    | otherwise           = isSuffixOf (x:xs) ys
    where
        checa :: Eq a => [a] -> [a] -> Bool
        checa [] []             = True
        checa [] _              = False
        checa _ []              = False
        checa (x:xs) (y:ys)
                    | x == y    = checa xs ys
                    | otherwise = False


zip :: [a] -> [b] -> [(a,b)]
zip [] []         = []
zip _  []         = []
zip [] _          = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] []         = []
zipWith f [] _          = []
zipWith f _  []         = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys


intercalate :: [a] -> [a]
intercalate []       = []
intercalate [x]      = [x]
intercalate (x:y:xs) = y : x : intercalate xs


nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs) = x : filter (/= x) (nub xs)


splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 xs     = ([],xs)
splitAt _ []     = ([],[])
splitAt n xs = (take n xs, drop n xs)


-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)
-- I don't know

-- break

-- lines


words :: [Char] -> [[Char]]
words [] = []
words xs = rmempty $ nub $  takeWhile (/=' ') xs : takeWhile (/=' ')  (drop (lEn xs) xs) : [] ++ words (drop (lEn xs) xs)
        where
            lEn :: [Char] -> Int
            lEn = (1 +) . length . takeWhile (/=' ') -- $ xs
            -- a funçõe à seguir é do livro do Hutton
            rmempty :: Eq a => [[a]] -> [[a]]
            rmempty = filter (/= [])


-- unlines

 
unwords :: [String] -> String
unwords []     = []
unwords (x:[]) = x
unwords (x:xs) = x ++ " " ++ unwords xs

-- transpose


-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome [] = True 
palindrome xs = True
--         | take (length x / 2) x == drop (length x / 2) x = True
--         | otherwise                                      = False
         where
             (/) :: Int -> Int -> Int
             (/) x 0         = error "não se divide por 0"
             (/) x y
                 | x < y     = 0
                 | otherwise = 1 + (/) (x-y) y
             clean :: [Char] -> [Char]
             clean [] = []
             clean (x:xs)
               | x `elem` abc  = x : clean xs
               | x `elem` abc' = x : clean xs
               | otherwise = clean xs
             abc = ['a'..'z']
             abc' = ['A'..'Z']

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

