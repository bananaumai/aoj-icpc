-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2252

import           Control.Monad
import           Data.List     as List
import           Data.Maybe
import           Data.Set      as Set

data Hand = L | R deriving (Show, Eq)
type Key = String

leftKeys :: Set String
leftKeys = fromList $ List.map (:[]) "qwertasdfgzxcvb"

whichHand :: Key -> Hand
whichHand k = if isJust (lookupIndex k leftKeys) then L else R

count :: [Key] -> Hand -> Int -> Int
count [] _ cnt = cnt
count (k:ks) prevHand cnt = count ks currentHand (calc cnt)
  where
    currentHand = whichHand k
    calc c = if prevHand == currentHand then c else cnt+1

solve :: String -> Int
solve str = let (k:ks) = List.map (:[]) str
            in count ks (whichHand k) 0

main :: IO ()
main = do
  str <- getLine
  unless (str == "#") $ do
    print $ solve str
    main
