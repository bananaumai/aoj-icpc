import           Debug.Trace

main :: IO ()
main = print $ solve 367 186 151
--main = print $ isPrime 73

solve :: Int -> Int -> Int -> Int
solve a d n = filter (isPrime . fromIntegral) [a,(a+d)..] !! (n-1)

isPrime :: Integer -> Bool
isPrime n = isPrime' (n-1) where
  isPrime' :: Integer -> Bool
  isPrime' x
    | x == 1             = trace (show n) True
    | (x^n `mod` n) /= x = False
    | otherwise          = isPrime' (x-1)
