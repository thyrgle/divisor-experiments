module Lib
    ( nthPentagonal,
      pentagonals,
      divisorSum,
    ) where

-- | Creates a [generalized pentagonal integer]
-- | (https://en.wikipedia.org/wiki/Pentagonal_number_theorem) integer.
nthPentagonal :: Integer -> Integer
nthPentagonal n = n * (3 * n - 1) `div` 2


-- | Creates a lazy list of all the pentagonal numbers.
pentagonals :: [Integer]
pentagonals = map nthPentagonal integerStream

-- | Provides a stream for representing a bijection from naturals to integers
-- | i.e. [1, -1, 2, -2, ... ].
integerStream :: [Integer]
integerStream = concatMap mirror [1 ..]
    where
      mirror n = [n, -n]

-- | Using Euler's formula for the divisor function, we see that each summand
-- | alternates between two positive and two negative. This provides a stream
-- | of 1 1 -1 -1 1 1 ... to utilze in assiting this property.
additiveStream :: [Integer]
additiveStream = cycle [1, 1, -1, -1]

-- | Kronkecker delta, return 0 if the integers are not the same, otherwise,
-- | return the value of the integer.
delta :: (Eq n, Num n) => n -> n -> n
delta n i
    | n == i = n
    | otherwise = 0

-- | Calculate the sum of the divisors.
-- | Utilizes Euler's recurrence formula:
-- | $\sigma(n) = \sigma(n - 1) + \sigma(n - 2) - \sigma(n - 5) \ldots $
-- | See [here](https://math.stackexchange.com/a/22744/15140) for more informa-
-- | tion.
divisorSum :: Integer -> Integer
divisorSum n
    | n <= 0 = 0
    | otherwise = sum $ takeWhile (/= 0)
                                  (zipWith (+)
                                           (divisorStream n)
                                           (markPentagonal n))
    where
    pentDual n = [ n - x | x <- pentagonals]
    divisorStream n = zipWith (*)
                              (map divisorSum (pentDual n))
                              additiveStream
    markPentagonal n = zipWith (*)
                               (zipWith (delta) 
                                        pentagonals
                                        (repeat n))
                               additiveStream
