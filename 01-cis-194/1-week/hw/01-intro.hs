-- Exercise 1

-- toDigits converts positive Integers to a list of digits
-- (For 0 or negative inputs, toDigits should return the empty list.)
-- Example: 1386 becomes [1,3,8,6]
-- Solution found: https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

-- toDigitsRev converts positive Integers to a reversed list of digits
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2

-- doubleEveryOther doubles every other number beginning from the right
-- Example: [1,3,8,6] becomes [2,3,16,6].
-- Solution found: https://stackoverflow.com/questions/17383169/haskell-double-every-2nd-element-in-list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:xs) = x : (2 * head xs) : doubleEveryOther (tail xs)

-- Exercise 3

-- sumDigits adds all digits together
-- Example: [2,3,16,6] becomes 2+3+1+6+6 = 18.
sumDigits :: [Integer] -> Integer
sumDigits x = sum [sum (toDigits a) | a <- x]

-- Exercise 4

-- validate validates a credit card number
-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0

-- Nate's Bonus Round
-- From reading Real World Haskell, let's make this a runnable binary
-- Run from CLI with `echo "4012888888881881" | runghc 01-intro`
main :: IO ()
main = interact cardNumber
    where cardNumber input = show (validate (read input :: Integer)) ++ "\n"

-- Note to future self:
-- This took a long time to complete ~90 minutes. I felt that I didn't have sufficient information
-- to come up with the ideal solutions like using map for exercise 1 and zipWith for exercise 2.
-- I enjoyed showing D how many errors I was getting in the ghci repl. Learning new languages is hard... :-)
-- This was a good first taste.
