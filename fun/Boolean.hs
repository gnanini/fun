module Boolean where


data Boolean = T
             | F
             deriving (Show)


lor :: Boolean -> Boolean -> Boolean
lor F F = F
lor _ _ = T


land :: Boolean -> Boolean -> Boolean
land T T = T
land _ _ = F


lnot :: Boolean -> Boolean
lnot T = F
lnot _ = T


ifthenelse :: Boolean -> a -> a -> a
ifthenelse T x _ = x
ifthenelse _ _ y = y
