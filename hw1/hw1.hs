-- ex1
toDigits :: Integer -> [Integer]
toDigits x = reverse(toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = mod x 10 : toDigitsRev(div x 10)

-- ex2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs) =
  if length(xs) `mod` 2 == 0
  then x : doubleEveryOther xs
  else x*2 : doubleEveryOther xs

-- ex3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) =
          if x `div` 10 < 1
          then x + sumDigits xs
          else sumDigits(toDigits x) + sumDigits(xs)

-- ex4
validate :: Integer -> Bool
validate x = sumDigits(doubleEveryOther(toDigits(x))) `mod` 10 == 0

-- ex5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
