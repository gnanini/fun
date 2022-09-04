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
safetail xs = if (null' xs) == True then tail xs else []

null' :: [a] -> Bool --define se uma lista é vazia ou não
--usando condicionais
null' xs = if length xs > 0 then True else False


safetailGuard :: [a] -> [a]
safetailGuard xs  | nullGuard xs = tail xs
                  | otherwise    = []

nullGuard :: [a] -> Bool
nullGuard xs  | length xs > 0 = True
              | otherwise     = False

-- tá errado
--safetailPattern :: [a] -> [a]
--safetailPattern xs = (\True -> tail xs) (nullPattern xs)
--safetailPattern xs = (\False -> xs) (nullPattern xs)
--safetailPattern xs = (\False -> xs) (nullPattern xs)


nullPattern :: [a] -> Bool
nullPattern []  = False
nullPattern _  = True

isPrime :: Integral a => a -> Bool
isPrime k = null ([x | x <- [2..(k`div`2+1)], k `mod` x == 0]) -- && k /= 1)
-- isPrime k = if [x | x <- [2..(k-1)], k `mod` x == 0] == [] && k /= 1 then True else False

primes :: Integral a => a -> [a]
primes k = [x | x <- [2..(k-1)], isPrime x]

-- esse gera o fatorial final
fat :: Integer -> Integer
fat k = product  [1..k]


-- esse gera uma lista de fatoriais
fatList :: Integer -> [Integer]
fatList k = map fatorial [1..k]

fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial(n-1)

junta :: a -> b -> (a, b)
junta x y = (x,y)


somaDuplos :: Num a => (a, a) -> a
somaDuplos (x,y) = x + y


teams :: [a] -> ([a],[a])
teams (x:[]) = ([], [x])
teams [x,y] = ([x],[y])
teams (x:y:xs) = (x:(recurl), (y:recurr))
        where (recurr, recurl) = teams xs
 

headTeams :: ([a], [a]) -> [a]
headTeams ([x],[y]) = [x,y]
headTeams ((x:xs),(y:ys)) = (x:y:recursion)
        where recursion = headTeams (xs,ys)


geraPrimos :: Integer -> [Integer]
geraPrimos k
         | k <= 1 = []
         | x <= k && naoDiv x (xs++[x]) = xs
            where x = 2
                  xs = [2]

--        | k < 2 = []
--        | k `div` 2 == 1       = [k]
--        | k `mod` (k `div` 2) == 0 = 
--geraPrimos [] = []
--geraPrimos k
--        | k <= 1 = []
--        | k == 2 = 2
--        | k `mod` geraPrimos (k - 1) == 0 = []
--        | otherwise



-- retorna True se [x..]  ¬ divisível por k
naoDiv :: Integer -> [Integer] -> Bool
naoDiv _ [0] = True
naoDiv k [x]
        | x `mod` k /= 0 = True
naoDiv k (x:xs)
        | x `mod` k == 0 = False
        | otherwise = naoDiv k xs



-- daqui pra baixo é fullstackoverflow
fibonacci :: Int -> [Int]
fibonacci k = take k (1 : 1 : zipWith (+) fibs (tail fibs))

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

--fibo :: Integer, Integer -> [Integer]
fibo a b = a:fibo b (a+b)

