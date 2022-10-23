module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

-- define Rat:
data Rat = Rat Integer Integer

instance Show Rat where
    show (Rat x y) = show x ++ "/" ++ show y


instance Eq Rat where
    (==) (Rat n m) (Rat p q)
        | n `div` m == p `div` q = True
        | otherwise = False


instance Num Rat where
    (+) (Rat x y) (Rat z w)
        | y == w    = Rat (x+z) y
        | otherwise = Rat (x*w) (y*w) + Rat (z*y) (w*y)

    (*) (Rat x y) (Rat z w) = Rat (x*z) (y*w)

    negate (Rat x y) = Rat x y * Rat (-1) 1

    abs (Rat x y) = Rat (abs x) (abs y)

    signum (Rat x y)
        | x > 0 && y > 0 = Rat 1 1
        | x < 0 || y < 0 = Rat (-1) 1
        | otherwise = Rat 0 1

    fromInteger x = rat x 1

instance Ord Rat where
    compare  (Rat x y) (Rat z w)
        | Rat x y == Rat z w = EQ
        | Rat x y > Rat z w  = GT
        | otherwise          = LT

rat :: Integer -> Integer -> Rat
rat x y
    | y == 0    = error "não pode"
    | otherwise = simp 2 (Rat x y)
    where 
        simp :: Integer -> Rat -> Rat
        simp n r@(Rat p q)
            | n > q             = r
            | p `mod` n == 0 && q `mod` n == 0 = simp n (Rat (p `div` n) (q `div` n))
            | otherwise          = simp (n+1) r

(//) :: Rat -> Rat -> Rat
(//) x y = rat (n x * d y) (d x * n y)
        where
            d = denominator
            n = numerator

denominator :: Rat -> Integer
denominator (Rat _ x) = x

numerator :: Rat -> Integer
numerator (Rat x _) = x
