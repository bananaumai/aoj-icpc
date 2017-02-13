-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2440&lang=jp
import           Control.Applicative
import           Control.Monad
import           Data.List           as List
import           Data.Set            as Set

main :: IO ()
main = do
  n <- readLn
  us <- replicateM n getLine
  m <- readLn
  ts <- replicateM m getLine
  putStr $ unlines $ List.map message $ solve (fromList us) ts Close

data State  = Open | Close deriving (Eq, Show)
data Result = Opened | Closed | Unknown deriving Show
type ID = String

solve :: Set ID -> [ID] -> State -> [(Result, ID)]
solve us ts s = solve' us ts s [] where
  solve' _ [] _ acc = acc
  solve' us ts s acc = let t = head ts
                           (s', r) = check us t s
                        in solve' us (tail ts) s' $ acc ++ [(r, t)]

check :: Set ID -> ID -> State -> (State, Result)
check us t s = case lookupIndex t us of
                    Just i -> if s == Open then (Close, Closed) else (Open, Opened)
                    Nothing -> (s, Unknown)

message :: (Result, ID) -> String
message (r, i) = case r of Opened -> "Opened by " ++ i
                           Closed -> "Closed by " ++ i
                           Unknown -> "Unknown " ++ i
