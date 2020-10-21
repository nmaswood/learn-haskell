module Main where

import Data.List (intercalate)
import Text.Read (readMaybe)

data Player = X | O deriving (Show, Eq)

data Spot = Spot Player | E deriving (Show, Eq)

data Row = Row Spot Spot Spot deriving (Show, Eq)

data Board = Board Row Row Row deriving (Show, Eq)

newtype BoardIndex = BoardIndex Integer

data Point = Point BoardIndex BoardIndex

data GameState = GameState Board Player

emptyBoard :: Board
emptyBoard = Board emptyRow emptyRow emptyRow

emptyRow :: Row
emptyRow = Row E E E

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

gameLoop :: GameState -> IO ()
gameLoop (GameState board p) = do
  putStrLn "Row:\n"
  row <- getLine
  putStrLn "Col:\n"
  col <- getLine
  pointFromUser <- validPointFromUser row col board
  case pointFromUser of
    Just point -> gameLoop (assignPointToPlayer p point board) (nextPlayer p)
    Nothing -> do
      putStrLn "Please input a valid col / row index"
      gameLoop (GameState b p)

initGameState :: GameState
initGameState = GameState emptyBoard X

gameStateToString :: GameState -> String
gameStateToString (GameState (Board r1 r2 r3) p) =
  intercalate ("\n") [rowToString r1, rowToString r2, rowToString r3]

rowToString :: Row -> String
rowToString (Row r1 r2 r3) =
  intercalate (" ") [(show r1), (show r2), (show r3)]

validPointFromUser :: String -> String -> Board -> Maybe Point
validPointFromUser r c b = do
  inputPoint <- pointFromUser r c
  validatePoint b inputPoint

validatePoint :: Board -> Point -> Maybe Point
validatePoint b p =
  case elem p (availablePointsForBoard b) of
    True -> Just p
    False -> Nothing

pointFromUser :: String -> String -> Maybe Point
pointFromUser r c = do
  rAsInt <- (readMaybe r :: Maybe Integer)
  cAsInt <- (readMaybe c :: Maybe Integer)
  pointFromIntegers rAsInt cAsInt

pointFromIntegers :: Integer -> Integer -> Maybe Point
pointFromIntegers r c = do
  rBoardIndex <- boardIndexFromInteger r
  cBoardIndex <- boardIndexFromInteger c
  Just (Point rBoardIndex cBoardIndex)

boardIndexFromInteger :: Integer -> Maybe BoardIndex
boardIndexFromInteger 0 = Just (BoardIndex 0)
boardIndexFromInteger 1 = Just (BoardIndex 1)
boardIndexFromInteger 2 = Just (BoardIndex 2)
boardIndexFromInteger _ = Nothing

hasPlayerWon :: Player -> Board -> Bool
hasPlayerWon player (Board (Row r1 r2 r3) (Row r4 r5 r6) (Row r7 r8 r9)) =
  listEqualToPlayer player [r1, r2, r3]
    || listEqualToPlayer player [r4, r5, r6]
    || listEqualToPlayer player [r7, r8, r9]
    || listEqualToPlayer player [r1, r4, r7]
    || listEqualToPlayer player [r2, r5, r8]
    || listEqualToPlayer player [r3, r6, r9]
    || listEqualToPlayer player [r1, r5, r9]
    || listEqualToPlayer player [r3, r5, r7]

listEqualToPlayer :: Player -> [Spot] -> Bool
listEqualToPlayer player spots = all (spotEqualToPlayer player) spots

spotEqualToPlayer :: Player -> Spot -> Bool
spotEqualToPlayer player (Spot s) = player == s

assignPointToPlayer :: Player -> Point -> Board -> Board
assignPointToPlayer player (Point rowIndex colIndex) (Board r1 r2 r3) = case rowIndex of
  BoardIndex 0 -> Board (assignPointToPlayerForRow player colIndex r1) r2 r3
  BoardIndex 1 -> Board r1 (assignPointToPlayerForRow player colIndex r2) r3
  BoardIndex 2 -> Board r1 r2 (assignPointToPlayerForRow player colIndex r3)
  _ -> error "Invalid board index"

assignPointToPlayerForRow :: Player -> BoardIndex -> Row -> Row
assignPointToPlayerForRow player boardIndex (Row r1 r2 r3) = case boardIndex of
  BoardIndex 0 -> Row (Spot player) r2 r3
  BoardIndex 1 -> Row r1 (Spot player) r3
  BoardIndex 2 -> Row r1 r2 (Spot player)
  _ -> error "Invalid board index"

availablePointsForBoard :: Board -> [Point]
availablePointsForBoard (Board r1 r2 r3) = (availablePointsForRow (BoardIndex 0) r1) ++ (availablePointsForRow (BoardIndex 1) r2) ++ (availablePointsForRow (BoardIndex 2) r3)

availablePointsForRow :: BoardIndex -> Row -> [Point]
availablePointsForRow index row = case row of
  (Row E E E) -> [(Point index (BoardIndex 0)), (Point index (BoardIndex 1)), (Point index (BoardIndex 2))]
  (Row E E _) -> [(Point index (BoardIndex 0)), (Point index (BoardIndex 1))]
  (Row E _ E) -> [(Point index (BoardIndex 0)), (Point index (BoardIndex 2))]
  (Row _ E E) -> [(Point index (BoardIndex 1)), (Point index (BoardIndex 2))]
  (Row E _ _) -> [(Point index (BoardIndex 0))]
  (Row _ E _) -> [(Point index (BoardIndex 1))]
  (Row _ _ E) -> [(Point index (BoardIndex 2))]
  _ -> []

main :: IO ()
main = do
  putStrLn "Welcome! Let's Pay "
  x <- getLine
  print x
