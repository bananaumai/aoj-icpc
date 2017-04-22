import           Control.Applicative
import           Control.Monad

main :: IO ()
main = do
  [d, e] <- map read . words <$> getLine :: IO [Int]
  unless (d == 0 && e == 0) $ do
    print $ solve d e
    main

solve :: Int -> Int -> Double
solve d e = minimum $ map (diff e) (pairs d)

pairs :: Int -> [(Int, Int)]
pairs d
  | d == 1 = [(0, 1)]
  | even 2 = [0..h] `zip` [d,d-1..h]
  | odd 2  = [0..h] `zip` [d,d-1..h+1]
  where h = d `div` 2

diff :: Int -> (Int, Int) -> Double
diff e (x, y) = let e' = fromIntegral e :: Double
                    x' = fromIntegral x :: Double
                    y' = fromIntegral y :: Double
                 in abs $ sqrt (x' ^^ 2 + y' ^^ 2) - e'
