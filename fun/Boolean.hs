module Boolean where


data Boolean = T
             | F
             deriving (Show)


lor :: Boolean -> Boolean -> Boolean
lor F F = F
lor _ _ = T


land T T = T
land _ _ = F
