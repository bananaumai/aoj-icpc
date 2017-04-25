import           Control.Applicative
import           Control.Monad

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  ss <- replicateM n getLine
  putStrLn $ solve ss

solve :: [String] -> String
solve ss = solve' ss []
  where solve' :: [String] -> [String] -> String
        solve' [] [] = "YES"
        solve' [] _ = "NO"
        solve' ("Un":_) [] = "NO"
        solve' ("Un":ss) (_:as) = solve' ss as
        solve' ("A":ss) as = solve' ss ("A":as)
