-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1179&lang=jp
import           Control.Applicative
import           Control.Monad

daysInYear :: Int -> Int
daysInYear year = if year `mod` 3 == 0
                  then 10 * 20
                  else 5 * 20 + 5 * 19

daysInMonth :: Int -> Int -> Int
daysInMonth y m = if odd m || y `mod` 3 == 0
                  then 20
                  else 19

solve :: [Int] -> Int
solve [y, m, d] =
  let yearsLeft   = [y+1..999]
      monthsLeft  = [m+1..10]
      daysLeft    = daysInMonth y m + 1 - d
  in daysLeft +
     foldl (\acc m -> acc + daysInMonth y m) 0 monthsLeft +
     foldl (\acc y -> acc + daysInYear y) 0 yearsLeft

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  ds <- replicateM n $ solve . map read . words <$> getLine
  putStr $ unlines $ map show ds

