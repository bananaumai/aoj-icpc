-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1192&lang=jp
import           Control.Applicative
import           Control.Monad

main :: IO ()
main = do
  [x, y, s] <- map read . words <$> getLine :: IO [Int]
  unless ([x,y,s] == [0,0,0]) $ do
    print $ solve x y s
    main

solve :: Int -> Int -> Int -> Int
solve x y totalPrice = let ps = pairs totalPrice x
                        in maximum $ map (\p -> calc (fst p) y + calc (snd p) y) ps

pairs :: Int -> Int -> [(Int, Int)]
pairs totalPrice taxRate = [(a, b) | a <- [1..(totalPrice-1)],
                                     b <- [1..(totalPrice-1)],
                                     calc a taxRate + calc b taxRate == totalPrice]

calc :: Int -> Int -> Int
calc price taxRate = price * (100 + taxRate) `div` 100
