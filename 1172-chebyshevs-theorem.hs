import           Control.Monad
import           Data.List     as List
import           Data.Set      as Set

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  unless (n == 0) $ do
    print $ length $ List.filter (>=n) $ primes [2..(n*2)]
    main

primes :: [Int] -> [Int]
primes [] = []
primes (x:xs) = x : primes [y | y <- xs, mod y x /= 0]
