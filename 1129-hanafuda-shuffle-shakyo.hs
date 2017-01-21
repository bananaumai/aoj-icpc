-- this is shakyo of http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=2031642#1
import Control.Applicative ((<$>))
import Control.Monad

main :: IO ()
main = do
  [n, r] <- getl $ wrds toInt
  unless (n == 0 && r == 0) $ do
    solve n <$> replicateM r (getl $ wrds toInt) >>= print
    main

solve :: Int -> [[Int]] -> Int
solve n pcs = head $ foldl sfl [n, n-1 .. 1] pcs
  where
    sfl ns [p, c] = cs ++ ps ++ rs
      where
        (ps, ts) = splitAt (p-1) ns
        (cs, rs) = splitAt c ts

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine