-- stoi
stoi :: String -> Int
stoi a = read $ a :: Int

-- gcd default
-- gcd a b

-- gcd custom
__gcd :: Int -> Int -> Int
__gcd 0 a = a
__gcd a 0 = a
__gcd a b
    | a > b = __gcd b a
    | otherwise = __gcd (b `mod` a) a
    
-- lcm custom
__lcm :: Int -> Int -> Int
__lcm a b = a * b `div` (__gcd a b)

-- check prime
isPrime :: Int -> Bool
isPrime a = loopFunc a 2
    where    loopFunc :: Int -> Int -> Bool
	     loopFunc 2 i = True 
	     loopFunc 3 i = True
	     loopFunc a i
	         | a <= 1 = False
	         | i == a = True
	         | a `mod` i == 0 = False
	         | otherwise = loopFunc a (i+1)
           
-- count digit of number
cntDigit :: Int -> Int
cntDigit a = helper a 0
    where    helper :: Int -> Int -> Int
 	     helper a cur
	         | a < 10 = cur + 1
		 | otherwise = helper (a `div` 10) (cur + 1)
     
-- sumPrime <= n
sumPrime :: Int -> Int
sumPrime n = sum [i | i <- [1..n], isPrime i]

