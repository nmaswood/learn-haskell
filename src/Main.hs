module Main where

data Player = X | O deriving (Show, Eq)

data Spot = Spot Player | E deriving (Show, Eq)

data Row = Row Spot Spot Spot deriving (Show, Eq)

data Board = Board Row Row Row deriving (Show, Eq)

newtype BoardIndex = BoardIndex Integer

data Point = Point BoardIndex BoardIndex

data GameStatus = WIN | LOSS | ONGOING

data GameState = GameState Board Player GameStatus

emptyBoard :: Board
emptyBoard = Board (Row E E E) (Row E E E) (Row E E E)

initGameState :: GameState
initGameState = GameState emptyBoard X ONGOING

boardIndexFromInteger :: Integer -> Maybe BoardIndex
boardIndexFromInteger 0 = Just (BoardIndex 0)
boardIndexFromInteger 1 = Just (BoardIndex 1)
boardIndexFromInteger 2 = Just (BoardIndex 2)
boardIndexFromInteger _ = Nothing

main :: IO ()
main = do
  putStrLn (show (1 + 1))
