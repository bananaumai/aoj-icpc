{-# LANGUAGE ViewPatterns #-}
import           Control.Applicative
import           Control.Monad       as Monad
import           Data.List           as List
import           Data.Map            as Map
import           Data.Sequence       as Seq
import           Data.Set            as Set
import           Debug.Trace

main = do
  [w,h] <- List.map (read :: String -> Int) . words <$> getLine
  unless ([w,h] == [0,0]) $ do
    lines <- Monad.replicateM h $ List.map (read :: String -> Int) . words <$> getLine
    print $ solve $ Map.fromList $ [(x,y) | y <- [1..h], x <- [1..w]] `List.zip` concat lines
    main

type Coord = (Int, Int)

solve :: Map Coord Int -> Int
solve m = let lands = Set.fromList $ Map.keys $ Map.filter (==1) m
           in seek m lands 0

seek :: Map Coord Int -> Set Coord -> Int -> Int
seek m lands count
  | Set.null lands = count
  | otherwise      = let land = Set.elemAt 0 lands
                         lands' = Set.delete land lands
                         lands'' = del m (Seq.fromList [land]) lands' Set.empty
                         count' = count + 1
                      in seek m lands'' count'

del :: Map Coord Int -> Seq Coord -> Set Coord -> Set Coord -> Set Coord
del _ (viewl -> EmptyL) lands _ = lands
del m (viewl -> c@(x,y) :< q) lands deleted
  | Set.member c deleted = del m q lands deleted
  | otherwise            = case Map.lookup c m of Nothing -> next
                                                  Just 0  -> next
                                                  Just 1  -> del m q' (Set.delete c lands) (Set.insert c deleted)
  where q' = q |> (x+1,y) |> (x+1,y+1) |> (x+1,y-1) |> (x-1,y) |> (x-1,y+1) |> (x-1,y-1) |> (x,y+1) |> (x,y-1)
        next = del m q lands deleted
