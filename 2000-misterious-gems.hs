-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2000&lang=jp
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.List           as List
import           Data.Set            as Set

main :: IO ()
main = do
  n <- toInt <$> getLine
  unless (n == 0) $ do
    gems <- replicateM n $ makeGemCoord <$> getLine
    m <- toInt <$> getLine
    mvnts <- replicateM m $ makeMvnt <$> getLine
    putStrLn $ solve (fromList gems) mvnts (10, 10)
    main

type Movement = (String, Int)
type Coord = (Int, Int)

toInt :: String -> Int
toInt = read

makeGemCoord :: String -> Coord
makeGemCoord s = convPair s $ \p -> (read *** read) p

makeMvnt :: String -> Movement
makeMvnt s = convPair s $ \p -> second read p

convPair :: String -> ((String, String) -> (a, b)) -> (a, b)
convPair s f = case words s of { x : y : _ -> f (x, y) }

solve :: Set Coord -> [Movement] -> Coord -> String
solve gs ms c
  | Set.null gs = "Yes"
  | List.null ms = "No"
  | otherwise = solve nextGems nextMvnts nextCoord
  where
    nextGems = Set.delete nextCoord gs
    m = head ms
    nextMvnts = if snd m - 1 == 0 then tail ms else (fst m, snd m - 1):tail ms
    nextCoord = case fst m of
                  "N" -> (fst c, snd c + 1)
                  "S" -> (fst c, snd c - 1)
                  "E" -> (fst c + 1, snd c)
                  "W" -> (fst c - 1, snd c)
