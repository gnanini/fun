iseven :: Integral a => a -> Bool
iseven n = n `mod` 2 == 0


test :: [Char] -> Bool
test ('a':_) = True
test ('A':_) = True
test _ = False

testA a | head a == 'a' = True
        | head a == 'A' = True
        | otherwise = False

testB a = 
  if head a == 'b'
     then True
  else
    if head a == 'B'
       then True
    else False

halve :: [a] -> ([a],[a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)


third :: [a] -> a
third xs = head $ tail $ tail $ xs


thirdIndex :: [a] -> a
thirdIndex xs = xs !! 2

thirdPattern :: [a] -> a
--thirdPattern [_,_,x:_] = x
thirdPattern (_:(_:(x:_))) = x


halveIf :: [a] -> ([a],[a])
halveIf xs = (take (metade xs) xs, drop (metade xs) xs)

-- metade vai entrar no halveIF
metade :: [a] -> Int
metade xs =
  if (length xs) `mod` 2 /= 0
     then (length xs) `div` 2 + 1
  else (length xs) `div` 2


safetail :: [a] -> [a]
safetail xs = if (Main.null xs) == True then tail xs else []

null :: [a] -> Bool --define se uma lista é vazia ou não
--usando condicionais
null xs = if length xs > 0 then True else False


safetailGuard :: [a] -> [a]
safetailGuard xs  | nullGuard xs = tail xs
                  | otherwise    = []

nullGuard :: [a] -> Bool
nullGuard xs  | length xs > 0 = True
              | otherwise     = False

-- tá errado
safetailPattern :: [a] -> [a]
safetailPattern xs = (\True -> tail xs) (nullPattern xs)
safetailPattern xs = (\False -> xs) (nullPattern xs)
--safetailPattern xs = (\False -> xs) (nullPattern xs)


nullPattern :: [a] -> Bool
nullPattern []  = False
nullPattern (_)  = True

isPrime k = if [x | x <- [2..(k-1)], k `mod` x == 0] == [] && k /= 1 then True else False

primes k = [x | x <- [2..(k-1)], isPrime x]

-- esse gera o fatorial final
fatorial' :: Integer -> Integer
fatorial' k = product [x | x <- [1..k]]


-- esse gera uma lista de fatoriais
fatorial :: Integer -> [Integer]
fatorial k = map fatorial' [1..k]


-- daqui pra baixo é fullstackoverflow
fibonacci :: Int -> [Int]
fibonacci k = take k (1 : 1 : zipWith (+) fibs (tail fibs))

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

--fibo :: Integer, Integer -> [Integer]
fibo a b = a:fibo b (a+b)

