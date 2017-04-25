import           Control.Applicative
import           Control.Monad
import           Data.List           as List

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  unless (n == 0) $ do
    ns <- map read . words <$> getLine :: IO [Int]
    print $ solve ns
    main

solve :: [Int] -> Int
solve ns = minimum $ solve' (sort ns) []
  where solve' :: [Int] -> [Int] -> [Int]
        solve' (n:ns) acc
          | null ns = acc
          | otherwise = solve' ns ((head ns - n):acc)
