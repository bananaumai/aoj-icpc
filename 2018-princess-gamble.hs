-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2018&lang=jp

import           Control.Applicative
import           Control.Monad

calc :: Int -> Int -> Int -> Int
calc t n dr = let gold = realToFrac t * 100.0 / realToFrac n
                  rate = realToFrac dr / 100.0
              in floor $ gold - gold * rate

main :: IO ()
main = do
  [n, m, p] <- map read . words <$> getLine :: IO [Int]
  unless (n == 0 && m == 0 && p == 0) $ do
    xs <- replicateM n $ read <$> getLine :: IO [Int]
    print $ calc (sum xs) (xs !! (m-1)) p
    main
