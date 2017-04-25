import           Control.Applicative
import           Control.Monad

main :: IO ()
main = do
  [r0, w0, c, r] <- map read . words <$> getLine :: IO [Int]
  unless ([r0, w0, c, r] == [0, 0, 0, 0]) $ do
    print $ solve r0 w0 c r
    main

solve :: Int -> Int -> Int -> Int -> Int
solve r0 w0 c r
  | r0 `div` w0 >= c = 0
  | otherwise = let w = w0 * c - r0
                    rr = w `div` r
                 in if w `mod` r == 0 then rr else rr + 1
