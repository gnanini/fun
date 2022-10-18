module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

-- define Rat:
data Rat = Rat Integer Integer

instance Show Rat where
    --show Rat = "/" 
    show (Rat x y) = show x ++ "/" ++ show y

instance Eq Rat where
    (==) x y
        | rat (n x) (d x) - rat (n y) (d y) == 0 = True
        | otherwise = False
        where
            d = denominator
            n = numerator

instance Num Rat where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Ord Rat where
    compare = undefined

rat :: Integer -> Integer -> Rat
rat x y
    | y == 0     = error "não pode"
    | otherwise = Rat x y

(//) :: Rat -> Rat -> Rat
(//) x y = rat (n x * d y) (d x * n y)
        where
            d = denominator
            n = numerator

denominator :: Rat -> Integer
denominator (Rat _ x) = x

numerator :: Rat -> Integer
numerator (Rat x _) = x
