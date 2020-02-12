data Cell = Maybe Int | [Int] deriving (Show, Eq)

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

-}

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

{- checkColumn board val
Removes val from every other cell in the column corresponding to val

-}
checkColumn :: Board -> Cell -> Board
checkColumn board val = undefined


