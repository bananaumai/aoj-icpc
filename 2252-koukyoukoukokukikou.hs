-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2252
import           Control.Monad
import           Data.List     as List
import           Data.Maybe
import           Data.Set      as Set

data Hand = L | R deriving (Show, Eq)

whitchHand :: String -> Hand
whitchHand key = if isJust (lookupIndex key leftKeys) then L else R where
  leftKeys = fromList $ List.map (:[]) "qwertasdfgzxcvb"

count :: [String] -> Hand -> Int -> Int
count [] _ cnt = cnt
count (k:ks) prevHand cnt = count ks currentHand (calc cnt)
  where
    currentHand = whitchHand k
    calc c = if prevHand == currentHand then c else cnt+1

solve :: String -> Int
solve str = let (k:ks) = List.map (:[]) str
            in count ks (whitchHand k) 0

main :: IO ()
main = do
  str <- getLine
  unless (str == "#") $ do
    print $ solve str
    main
