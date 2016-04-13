{-# LANGUAGE FlexibleContexts #-}

module TicTacToe (main) where

import           Data.List
import           Data.Coerce
import           Data.Char
import           Control.Monad
import           Control.Monad.Reader

data Cell = O
          | X
          | N
  deriving (Eq)

instance Show Cell where
  show O = "0"
  show X = "x"
  show N = " "

data Winner = Computer
            | Player
            | Draw
  deriving (Eq, Show)

data Position = P Int Int
  deriving (Show, Eq)

data Board a = B !Cell !Cell !Cell !Cell !Cell !Cell !Cell !Cell !Cell

instance Show (Board a) where
  show (B x0 x1 x2 x3 x4 x5 x6 x7 x8) =
    show x0 ++
    " | " ++
    show x1 ++
    " | " ++
    show x2 ++
    "\n" ++
    "__________\n" ++
    show x3 ++
    " | " ++
    show x4 ++
    " | " ++
    show x5 ++
    "\n" ++
    "----------\n" ++
    show x6 ++
    " | " ++
    show x7 ++
    " | " ++
    show x8

data InPlay

data Empty

data Finished

class NotFinished a

instance NotFinished Empty

instance NotFinished InPlay

class NotEmpty a

instance NotEmpty InPlay

instance NotEmpty Finished

-- | takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a
-- new board
move :: (NotFinished state, MonadReader Cell m) => Board state -> Cell -> Position -> m (Either (Board InPlay) (Board Finished))
move board@(B c1 c2 c3 c4 c5 c6 c7 c8 c9) cell pos
  | n < 0 || n > 8 = error "Invalid position"
  | otherwise =
      do
        winner <- whoWon (coerce newBoard)
        if r /= N
          then return $ Left $ coerce board
          else if N `notElem` newCells || winner /= Draw
                 then return $ Right newBoard
                 else return $ Left newBoard
  where
    n = fromPos pos
    (l, r:rx) = splitAt n [c1, c2, c3, c4, c5, c6, c7, c8, c9]
    newCells@[c1', c2', c3', c4', c5', c6', c7', c8', c9'] = l ++ cell : rx
    newBoard = B c1' c2' c3' c4' c5' c6' c7' c8' c9'

-- | takes a tic-tac-toe board and returns the player that won the game (or a draw if neither)
whoWon :: MonadReader Cell m => Board Finished -> m Winner
whoWon (B x0 x1 x2 x3 x4 x5 x6 x7 x8) = do
  player <- ask
  return $ mconcat
             [ check x0 x1 x2 player
             , check x3 x4 x5 player
             , check x6 x7 x8 player
             , check x0 x3 x6 player
             , check x1 x4 x7 player
             , check x2 x5 x8 player
             , check x0 x4 x8 player
             , check x2 x4 x6 player
             ]

  where
    check x y z p
      | x /= y || y /= z = Draw
      | otherwise = getWinner x p

getWinner :: Cell -> Cell -> Winner
getWinner X X = Player
getWinner O O = Player
getWinner X O = Computer
getWinner O X = Computer
getWinner _ _ = Draw

-- | takes a tic-tac-toe board and position and returns the (possible) player at a given position
playerAt :: Board a -> Position -> Cell
playerAt (B c1 c2 c3 c4 c5 c6 c7 c8 c9) pos =
  case fromPos pos of
    0 -> c1
    1 -> c2
    2 -> c3
    3 -> c4
    4 -> c5
    5 -> c6
    6 -> c7
    7 -> c8
    8 -> c9
    _ -> error "Invalid position"

fromPos :: Position -> Int
fromPos (P x y) = 3 * x + y

instance Monoid Winner where
  mempty = Draw
  Draw `mappend` x = x
  x `mappend` _ = x

main :: IO ()
main = forever $ do
  putStrLn "Choose the player [X or O]: "
  input <- getLine
  case input of
    "X" -> runReaderT (playX new) X
    "O" -> runReaderT (playO new) O
    _   -> putStrLn "X or O"
  where
    new :: Board Empty
    new = B N N N N N N N N N

playX :: NotFinished state => Board state -> ReaderT Cell IO ()
playX board =
  eval board X (playX board) $ \b1 -> do
    liftIO $ print b1
    pos <- getBestPos b1 O
    exec b1 O pos playX

playO :: NotFinished state => Board state -> ReaderT Cell IO ()
playO board = do
  pos <- getBestPos board X
  exec board X pos $ \b1 -> eval b1 O (playO board) playO

eval :: NotFinished state
     => Board state
     -> Cell
     -> ReaderT Cell IO ()
     -> (Board InPlay -> ReaderT Cell IO ())
     -> ReaderT Cell IO ()
eval board cell retry next = do
  liftIO $ print board
  input <- lift getLine
  case toPosition input of
    Right position ->
      if isNotEmptyCell board position
        then liftIO (putStrLn "Not empty cell") >> retry
        else exec board cell position next
    Left message -> do
      liftIO $ putStrLn message
      eval board cell retry next

  where
    toPosition (x:' ':y:_)
      | isDigit x && isDigit y && digitToInt x < 9 && digitToInt y < 9 = Right $ P (digitToInt x)
                                                                                   (digitToInt y)
    toPosition _ = Left
                     "Position map (e.g. 1 1):\n0 0|0 1|0 2\n___________\n1 0|1 1|1 2\n-----------\n2 0|2 1|2 2\n"
    isNotEmptyCell b p = N /= playerAt b p

exec :: NotFinished state
     => Board state
     -> Cell
     -> Position
     -> (Board InPlay -> ReaderT Cell IO ())
     -> ReaderT Cell IO ()
exec board cell position action = do
  board' <- move board cell position
  case board' of
    Left b -> action b
    Right b -> do
      winner <- whoWon b
      liftIO $ putStrLn ("Game status: " ++ show winner ++ "\n" ++ show b)

getBestPos :: (NotFinished state, MonadReader Cell m) => Board state -> Cell -> m Position
getBestPos board cell = minimax board cell True

score :: MonadReader Cell m => Board Finished -> Int -> m Int
score board depth = do
  winner <- whoWon board
  case winner of
    Computer -> return $ 10 - depth
    Player   -> return $ depth - 10
    Draw     -> return 0

minimax :: (NotFinished state, MonadReader Cell m) => Board state -> Cell -> Bool -> m Position
minimax board cell flag = do
  let moves = getMoves board
  scores <- mapM (play board cell flag 0) moves
  case elemIndex (maxmin flag scores) scores of
    Just n -> return $ moves !! n
    Nothing -> error "minimax error"
  where
    maxmin True = maximum
    maxmin False = minimum

play :: (NotFinished state, MonadReader Cell m) => Board state -> Cell -> Bool -> Int -> Position -> m Int
play board cell flag depth position = do
  board' <- move board cell position
  case board' of
    Left b -> do
      let newCell = switch cell
      let newFlag = not flag
      newPos <- minimax b newCell newFlag
      play b newCell newFlag (depth + 1) newPos
    Right b ->
      score b depth

getMoves :: NotFinished state => Board state -> [Position]
getMoves (B c1 c2 c3 c4 c5 c6 c7 c8 c9) =
  toPos <$> N `elemIndices` [c1, c2, c3, c4, c5, c6, c7, c8, c9]

switch :: Cell -> Cell
switch X = O
switch O = X
switch _ = error "switch"

toPos :: Int -> Position
toPos n = P r c
  where
    (r, c) = n `divMod` 3
