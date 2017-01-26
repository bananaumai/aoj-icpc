import           Control.Applicative
import           Control.Monad

f :: [Int] -> Int -> Int -> Int
f ns len acc
  | length ns == 1        = acc
  | length ns - 1 < len   = f (tail ns) 2 acc
  | last ns < sm          = f (tail ns) 2 acc
  | last ns == sm         = f (tail ns) 2 (acc+1)
  | otherwise             = f ns (len+1) acc
  where sm = sum $ take len ns

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  unless (n == 0) $ do
    print $ f [1..n] 2 0
    main
