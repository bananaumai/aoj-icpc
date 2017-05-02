import           Control.Monad

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  unless (n == 0) $ do
    print $ solve n
    main

solve :: Int -> Int
solve n = length $ filter (>=n) $ theive (n * 2)

theive :: Int -> [Int]
theive n = theive' [2..n] []
  where theive' :: [Int] -> [Int] -> [Int]
        theive' [] acc = acc
        theive' (n:ns) acc = let acc' = n:acc
                              in theive' (filter (\x -> x `mod` n /= 0) ns) acc'
