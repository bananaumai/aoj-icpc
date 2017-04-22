import           Control.Applicative
import           Control.Monad

main = do
  n <- read <$> getLine :: IO Int
  unless (n == 0) $ do
    cmds <- words <$> getLine
    print $ solve (Down, Down) Up cmds 0
    main

data Pos = Up | Down deriving (Show, Eq)
type State = (Pos, Pos)
type Dest = Pos

solve  :: State -> Dest -> [String] -> Int -> Int
solve s d [] cnt = cnt
solve s d (cmd:cmds) cnt = solve nextState nextDest cmds nextCnt
  where nextState = step s cmd
        canIncrement = samePosition nextState d
        nextDest = if canIncrement then changeDest d else d
        nextCnt = if canIncrement then cnt + 1 else cnt

step :: State -> String -> State
step (lp, rp) p
  | p == "lu" = (Up, rp)
  | p == "ld" = (Down, rp)
  | p == "ru" = (lp, Up)
  | p == "rd" = (lp, Down)
  | otherwise = (lp, rp)

samePosition :: State -> Dest -> Bool
samePosition (sl, sr) d = sl == sr && sr == d

changeDest :: Dest -> Dest
changeDest Up = Down
changeDest Down = Up
