import           Control.Applicative
import           Control.Monad

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  unless (n == 0) $ do
    print $ solve n
    main

solve :: Int -> Int
solve n = let ps = thieve (n*2)
           in length $ filter (>n) ps

thieve :: Int -> [Int]
thieve n = thieve' [2..n] []
  where thieve' :: [Int] -> [Int] -> [Int]
        thieve' [x] acc = x:acc
        thieve' (x:xs) acc = if x^2 > n
                             then acc ++ xs
                             else thieve' (filter (\y -> y `mod` x /= 0) xs) (x:acc)
