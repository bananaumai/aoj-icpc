import           Control.Applicative
import           Control.Monad
import           Data.List

main :: IO ()
main = do
  [a, d, n] <- map (read :: String -> Int) . words <$> getLine
  unless ([a, d, n] == [0, 0, 0]) $ do
    print $ solve a d n
    main

limit :: Int
limit = 1000000

solve :: Int -> Int -> Int -> Int
solve a d n = [a,(a+d)..limit] `intersect` primes !! (n-1)

primes :: [Int]
primes = thieve limit

thieve :: Int -> [Int]
thieve n = thieve' [2..n] []
  where thieve' :: [Int] -> [Int] -> [Int]
        thieve' [x] acc = x:acc
        thieve' (x:xs) acc = if x^2 > n
                             then acc ++ xs
                             else thieve' (filter (\y -> y `mod` x /= 0) xs) (x:acc)
