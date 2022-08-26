dobreMe x = x + x


dobreNos x y = x*2 + y*2


dobrePequno x = if x < 100
                  then x*2
                  else x


--dobreLista ns = [ns] * 2


n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]


add :: Int -> (Int -> Int)
add x y = x+y

add' :: (Int, Int) -> Int
add' (x, y) = x + y


media xs = sum xs `div` length xs
