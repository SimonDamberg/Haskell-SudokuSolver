import Data.List
import Data.Traversable

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)

type Row = [Cell]

type Board = [Row]

{- makeBoard string
Creates a board with all possible values in cells from a string

-}
makeBoard :: String -> Board
makeBoard string = undefined

{- finishedBoard board
Checks if all cells are filled with one value-}
finishedBoard :: Board -> Bool
finishedBoard board = undefined

{- displayBoard board
Converts the board into graphical rows
-}
displayBoard :: Board -> String
displayBoard board = undefined

{- checkRow row val
Removes val from every other cell in row

-}
checkRow :: Row -> Cell -> Row
checkRow row val = undefined

{- checkSquare board val
Removes val from every other cell in the 3x3 square corresponding to val

-}
checkSquare :: Board -> Cell -> Board
checkSquare board val = undefined


