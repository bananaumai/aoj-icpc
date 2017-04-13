import           Control.Applicative
import           Control.Monad

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  unless (n == 0) $ do
    [w, h] <- map read . words <$> getLine :: IO [Int]
    ps <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
    [s, t] <- map read . words <$> getLine :: IO [Int]
    print $ solve w h (map (\[x, y] -> (x, y)) ps) s t
    main

type Coord = (Int, Int)

solve :: Int -> Int -> [Coord] -> Int -> Int -> Int
solve w h ps s t = maximum (trvs w h s t (1,1) ps [])

trvs :: Int -> Int -> Int -> Int -> Coord -> [Coord] -> [Int] -> [Int]
trvs w h s t (x, y) ps acc
  | t + y - 1> h = acc
  | s + x - 1 > w = trvs w h s t (1, y + 1) ps acc
  | otherwise = trvs w h s t (x + 1, y) ps (count (x, y) s t ps : acc)

count :: Coord -> Int -> Int -> [Coord] -> Int
count (x, y) s t ps = count' (x, y) s t ps 0
  where count' (x, y) s t [] acc = acc
        count' (x, y) s t ((px, py):ps) acc = if x <= px && px <= (x + s - 1) && y <= py && py <= (y + t - 1)
                                              then count' (x, y) s t ps (acc + 1)
                                              else count' (x, y) s t ps acc
