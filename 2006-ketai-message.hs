-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2006&lang=jp
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.List
import           Text.Read

eval :: (Int, Int) -> String
eval (n, c) = case n of
                1 -> case c `mod` 5 of
                      1 -> "."
                      2 -> ","
                      3 -> "!"
                      4 -> "?"
                      0 -> " "
                2 -> case c `mod` 3 of
                      1 -> "a"
                      2 -> "b"
                      0 -> "c"
                3 -> case c `mod` 3 of
                      1 -> "d"
                      2 -> "e"
                      0 -> "f"
                4 -> case c `mod` 3 of
                      1 -> "g"
                      2 -> "h"
                      0 -> "i"
                5 -> case c `mod` 3 of
                      1 -> "j"
                      2 -> "k"
                      0 -> "l"
                6 -> case c `mod` 3 of
                      1 -> "m"
                      2 -> "n"
                      0 -> "o"
                7 -> case c `mod` 4 of
                      1 -> "p"
                      2 -> "q"
                      3 -> "r"
                      0 -> "s"
                8 -> case c `mod` 3 of
                      1 -> "t"
                      2 -> "u"
                      0 -> "v"
                9 -> case c `mod` 4 of
                      1 -> "w"
                      2 -> "x"
                      3 -> "y"
                      0 -> "z"
                _ -> ""

toNums :: String -> [Int]
toNums = map (\c -> read [c])

solve :: String -> String
solve s = intercalate "" $ map (eval . (head &&& length)) $ group $ toNums s

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  ss <- replicateM n $ solve <$> getLine
  putStr $ unlines ss
