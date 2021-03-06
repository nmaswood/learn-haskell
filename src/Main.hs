module Main where

import Data.List (intercalate)
import Text.Read (readMaybe)

data Player = X | O deriving (Show, Eq)

data Spot = Spot Player | E deriving (Show, Eq)

data Row = Row Spot Spot Spot deriving (Show, Eq)

data Board = Board Row Row Row deriving (Show, Eq)

newtype BoardIndex = BoardIndex Integer deriving (Show, Eq)

data Point = Point BoardIndex BoardIndex deriving (Show, Eq)

data GameState = GameState Board Player

emptyBoard :: Board
emptyBoard = Board emptyRow emptyRow emptyRow

emptyRow :: Row
emptyRow = Row E E E

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

gameLoop :: GameState -> IO ()
gameLoop (GameState board p) =
  case hasPlayerWon p board of
    True -> putStrLn "The game has been won!"
    False -> do
      putStrLn (gameStateToString (GameState board p))
      putStrLn "\n\nSelect your row:\n"
      row <- getLine
      putStrLn "\n\nSelect your column:\n"
      col <- getLine
      case validPointFromUser row col board of
        Just point -> gameLoop (GameState (assignPointToPlayer p point board) (nextPlayer p))
        Nothing -> do
          putStrLn "Please input a valid col / row index"
          gameLoop (GameState board p)

initGameState :: GameState
initGameState = GameState emptyBoard X

gameStateToString :: GameState -> String
gameStateToString (GameState (Board r1 r2 r3) p) =
  "Player on deck: " ++ (show p) ++ "\n" ++ (intercalate ("\n") [rowToString r1, rowToString r2, rowToString r3])

rowToString :: Row -> String
rowToString (Row r1 r2 r3) =
  intercalate (" ") [(show r1), (show r2), (show r3)]

validPointFromUser :: String -> String -> Board -> Maybe Point
validPointFromUser r c b = do
  inputPoint <- pointFromUser r c
  validatePoint b inputPoint

validatePoint :: Board -> Point -> Maybe Point
validatePoint b p =
  let availablePoints = (availablePointsForBoard b)
   in case (elem p availablePoints) of
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
  any
    (listEqualToPlayer player)
    ( [ [r1, r2, r3],
        [r4, r5, r6],
        [r7, r8, r9],
        [r1, r4, r7],
        [r2, r5, r8],
        [r3, r6, r9],
        [r1, r5, r9],
        [r3, r5, r7]
      ]
    )

listEqualToPlayer :: Player -> [Spot] -> Bool
listEqualToPlayer player spots = all (spotEqualToPlayer player) spots

spotEqualToPlayer :: Player -> Spot -> Bool
spotEqualToPlayer player s = case s of
  Spot p -> p == player
  E -> False

assignPointToPlayer :: Player -> Point -> Board -> Board
assignPointToPlayer player (Point rowIndex colIndex) (Board r1 r2 r3) =
  let assignPoint = assignPointToPlayerForRow player colIndex
   in case rowIndex of
        BoardIndex 0 -> Board (assignPoint r1) r2 r3
        BoardIndex 1 -> Board r1 (assignPoint r2) r3
        BoardIndex 2 -> Board r1 r2 (assignPoint r3)
        _ -> error "Invalid board index"

assignPointToPlayerForRow :: Player -> BoardIndex -> Row -> Row
assignPointToPlayerForRow player boardIndex (Row r1 r2 r3) =
  let spot = Spot player
   in case boardIndex of
        BoardIndex 0 -> Row spot r2 r3
        BoardIndex 1 -> Row r1 spot r3
        BoardIndex 2 -> Row r1 r2 spot
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
  putStrLn "Welcome! Let's Pay Tic-Tac-Toe\n\n"
  gameLoop initGameState
