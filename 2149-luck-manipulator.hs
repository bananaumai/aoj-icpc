-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2149&lang=jp
import           Control.Applicative
import           Control.Monad
import           Data.List

main :: IO ()
main = do
  [n, a, b, c, x] <- map read . words <$> getLine :: IO [Int]
  unless ([n,a,b,c,x] == [0,0,0,0,0]) $ do
    ns <- map read . words <$> getLine :: IO [Int]
    print $ solve ns a b c x
    main


solve :: [Int] -> Int -> Int -> Int -> Int -> Int
solve ns a b c x = calc ns (rands a b c x) 0

calc :: [Int] -> [Int] -> Int -> Int
calc ns rnds acc
  | acc > 10000 = -1
  | null (delete (head rnds) ns) = acc
  | otherwise   = let rnd = head rnds
                      n = head ns
                      ns' = if n == rnd then tail ns else ns
                   in calc ns' (tail rnds) (acc+1)

rands :: Int -> Int -> Int -> Int -> [Int]
rands a b c x = x : rands a b c ((a * x + b) `mod` c)
