import           Control.Applicative
import           Control.Monad
import           Data.List           as List
import           Data.Set            as Set

main :: IO ()
main = do
  [n, q] <- List.map read . words <$> getLine :: IO [Int]
  unless ([n, q] == [0, 0]) $ do
    dts <- replicateM n $ fromList . drop 1 . List.map read . words <$> getLine :: IO [Set Int]
    print $ solve q (length dts) dts
    main

solve :: Int -> Int -> [Set Int] -> Int
solve q l dts
  | q == l    = if List.null days then 0 else minimum days
  | otherwise = if List.null days then solve q (l-1) dts else minimum days
  where combs = comb l dts
        days = List.filter (>0) $ List.map
                                  (\comb -> let ds = (toList . intersects) comb in if List.null ds then 0 else minimum ds)
                                  combs

intersects :: Ord a => [Set a] -> Set a
intersects = foldl1 intersection

comb :: Int -> [a] -> [[a]]
comb 0 _  = [[]]
comb _ [] =  []
comb n a@(x:xs)
  | length a == n = [a]
  | otherwise     = List.map (x:) (comb (n-1) xs) ++ comb n xs
