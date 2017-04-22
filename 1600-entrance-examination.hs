import           Control.Applicative
import           Control.Monad
import           Data.List

main = do
  [n, min, max] <- map read . words <$> getLine :: IO [Int]
  unless (n == 0 && min == 0 && max == 0) $ do
    ps <- replicateM n $ read <$> getLine :: IO [Int]
    print $ solve ps min max
    main

solve :: [Int] -> Int -> Int -> Int
solve ps min max = let ns = [min..max]
                       diffs = map (\n -> (ps !! (n-1)) - (ps !! n)) ns
                       idxs = elemIndices (maximum diffs) diffs
                    in ns !! maximum idxs
