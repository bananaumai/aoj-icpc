-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1129&lang=jp
main :: IO ()
main = do
 s <- getContents
 mapM_ print $ solve (strings2ds s) []

solve :: [[Int]] -> [[Int]] -> [Int]
solve ([0, 0]:_) acc = map head $ reverse acc
solve datasets acc = solve rest (result : acc) where
  numberOfCards = head (head datasets)
  numberOfConds = last (head datasets)
  cards = reverse [1..numberOfCards]
  conds = take numberOfConds $ drop 1 datasets
  rest = drop (numberOfConds + 1) datasets
  result = shuffle conds cards

strings2ds :: String -> [[Int]]
strings2ds s = map words2ns $ lines s

words2ns :: String -> [Int]
words2ns s = map read $ words s

shuffle :: [[Int]] -> [Int] -> [Int]
shuffle [] cards = cards
shuffle (cond:conds) cards = shuffle conds swapped where
  swapped = flatten . swap $ split (head cond) (last cond) cards

flatten :: [[Int]] -> [Int]
flatten = foldl concat []

swap :: [[Int]] -> [[Int]]
swap [xs:ys:zs] = [ys, xs, zs]

split :: Int -> Int -> [Int] -> [[Int]]
split p c l = [first : second : third] where
  first = take (p-1) l
  second = take c $ drop (p-1) l
  third = drop (p+c-1) l
