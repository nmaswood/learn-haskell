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
emptyBoard = Board emptyRow emptyRow emptyRow

emptyRow :: Row
emptyRow = Row E E E

initGameState :: GameState
initGameState = GameState emptyBoard X ONGOING

boardIndexFromInteger :: Integer -> Maybe BoardIndex
boardIndexFromInteger 0 = Just (BoardIndex 0)
boardIndexFromInteger 1 = Just (BoardIndex 1)
boardIndexFromInteger 2 = Just (BoardIndex 2)
boardIndexFromInteger _ = Nothing

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
   x <- getLine
   print x
