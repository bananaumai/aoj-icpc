-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1153&lang=jp

import           Control.Applicative
import           Control.Monad
import           Data.List           as List

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine :: IO [Int]
  unless (n == 0 && m == 0) $ do
    taro <- replicateM n $ read <$> getLine :: IO [Int]
    hanako <- replicateM m $ read <$> getLine :: IO [Int]
    putStrLn $ solve taro hanako
    main

solve :: [Int] -> [Int] -> String
solve taro hanako =
  let diff = sum taro - sum hanako
      pairs = sortOn sum [[t, h] | t <- taro, h <- hanako, t -h == diff `div` 2] -- use sortBy instead of sortOn in GHC 7
  in if odd diff
     then "-1"
     else case pairs of [] -> "-1"
                        p:_ -> unwords $ map show p
