import           Control.Applicative
import           Control.Monad
import           Data.List           as List
import           Data.Map            as Map
import           Data.Set            as Set

main :: IO ()
main = do
  [x, y] <- List.map (read :: String -> Int) . words <$> getLine
  unless ([x, y] == [0, 0]) $ do
    lines <- replicateM y getLine
    print $ solve x y (List.foldl (\acc line -> acc ++ line) "" lines)
    main

data Tile = Red | Black deriving (Eq, Show)
type Coord = (Int, Int)
type Room = Map Coord Tile
data Direction = Up | Down | Left | Right deriving (Eq, Show)

solve :: Int -> Int -> String -> Int
solve x y s = let cs = [(xi, yi) | yi <- [1..y], xi <- [1..x]]
                  room = makeRoom cs (List.map makeTile s)
                  c = case elemIndex '@' s of Just i -> cs !! i
               in Set.size $ move room c Set.empty

makeTile :: Char -> Tile
makeTile '#' = Red
makeTile _ = Black

makeRoom :: [Coord] -> [Tile] -> Room
makeRoom cs ts = Map.fromList $ cs `zip` ts

move :: Room -> Coord -> Set Coord -> Set Coord
move r c@(x,y) cs = case Map.lookup c r of Just Black -> move'
                                           _ -> cs
  where move' = if Set.member c cs
                then cs
                else List.foldl (\acc c -> move r c acc) (Set.insert c cs) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
