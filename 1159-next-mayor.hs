-- http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=1159
import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Sequence

main :: IO ()
main = do
  [n, p] <- map read . words <$> getLine :: IO [Int]
  unless (n == 0 && p == 0) $ do
    print $ solve (candidates n) p
    main

data Candidate = Candidate { getS :: Int, getN :: Int } deriving Show
type Candidates = Seq Candidate
type Cup = Int

candidates :: Int -> Candidates
candidates n = fromList $ map (Candidate 0) [0..n-1]

updateCandidate :: Candidate -> Cup -> Candidate
updateCandidate cdt 0 = Candidate { getN = getN cdt, getS = 0}
updateCandidate cdt _ = Candidate { getN = getN cdt, getS = getS cdt + 1}

solve :: Candidates -> Cup -> Int
solve cdts cup = if check then getN currentCdt else solve nextCdts currentCup where
  c :< restCdts = viewl cdts
  currentCdt = updateCandidate c cup
  currentCup = if cup == 0 then getS c else cup - 1
  -- check = Data.Foldable.all (\c -> getS c == 0) (toList restCdts) && currentCup == 0 -- GHC7系
  check = all (\c -> getS c == 0) restCdts && currentCup == 0 -- GHC8系
  nextCdts = restCdts |> currentCdt
