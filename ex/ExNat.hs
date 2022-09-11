module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show Zero     = "O"
    show (Succ x) = "S" ++ (show x)

instance Eq Nat where
    
--    (==) :: Eq Nat => Nat -> Nat -> Bool
    (==) Zero Zero         = True
    (==) _ Zero            = False
    (==) Zero _            = False
    (==) (Succ x) (Succ y) = (==) x y

instance Ord Nat where

    (<=) Zero Zero         = True
    (<=) (Succ _) Zero     = False
    (<=) Zero (Succ _)     = True
    (<=) (Succ x) (Succ y) = (<=) x y

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

--    min Zero _            = Zero
--    min _ Zero            = Zero
    min x y
        | x <-> y == Zero = x
        | otherwise       = y

    max x y
        | y <-> x == Zero = x
        | otherwise       = y

isZero :: Nat -> Bool
isZero Zero = True 
isZero _    = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero     = Zero
pred (Succ x) = x

even :: Nat -> Bool
even Zero            = True
even (Succ Zero)     = False
even (Succ (Succ x)) = even x

odd :: Nat -> Bool
odd Zero            = False
odd (Succ Zero)     = True
odd (Succ (Succ x)) = odd x

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) Zero x            = x 
(<+>) x Zero            = x
(<+>) (Succ x) (Succ y) = (<+>) x (Succ (Succ y))

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) x Zero            = x
(<->) Zero x            = Zero
(<->) (Succ x) (Succ y) = (<->) x y 

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) Zero _        = Zero 
(<*>) _ Zero        = Zero
(<*>) x (Succ Zero) = x
(<*>) (Succ Zero) x = x
(<*>) x (Succ y)    = ((<*>) x y <+> x)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) Zero _        = Zero
(<^>) _ Zero        = (Succ Zero)
(<^>) x (Succ Zero) = x
(<^>) (Succ Zero) x = (Succ Zero)
(<^>) x (Succ y)    = (((<^>) x y) <*> x)

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) Zero _        = Zero 
(</>) x  y
       | y <= x = (Succ ((</>) (x <-> y) y))
       | otherwise = Zero

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) x y = x <-> (x </> y * y)

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) x y
      | y <%> x == Zero = True
      | otherwise       = False

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff x y 
        | x <= y    = y <-> x
        | otherwise = x <-> y

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero     = (Succ Zero)
factorial (Succ x) = (Succ x) <*> (factorial x) 

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg _    = (Succ Zero)

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo Zero Zero       = Zero
lo _    1          = Zero
lo base n
      | base <|> n == True = (Succ (lo (n </> base) base))
      | otherwise          = error "número não divisível pela base"


--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat 0   = Zero
toNat x = (Succ (toNat (x - 1)))

fromNat :: Integral a => Nat -> a
fromNat Zero     = 0
fromNat (Succ x) = (1 + (fromNat (x)))


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = error "naturais não incluem números negativos"
        | x == 0    = Zero
        | otherwise = (Succ (fromInteger (x - 1)))
