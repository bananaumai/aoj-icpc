import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  unless (n == 0) $ do
    scores <- map read <$> replicateM n getLine :: IO [Int]
    print $ calc scores
    main

calc :: [Int] -> Int
calc ns = sum `div` len where
  sum = foldl (+) 0 $ take (length ns - 2) $ drop 1 $ sort ns :: Int
  len = length ns - 2