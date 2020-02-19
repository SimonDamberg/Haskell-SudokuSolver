import Data.List
import Data.List.Split
import Test.HUnit

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)

type Row = [Cell]

type Board = [Row]

{- finishedBoard board
Checks if all cells are filled with one value
-}
finishedBoard :: Board -> Bool
finishedBoard board = if length (foldl (++) "" (map finishedBoard' board)) == 81 then True else False
  where
    finishedBoard' [] = []
    finishedBoard' (x:xs) =
      case x of
        Fixed x -> "0" ++ finishedBoard' xs
        _ -> "" ++ finishedBoard' xs

{- makeBoard string
Creates a board with all possible values in cells from a string
-}
makeBoard :: String -> Maybe Board
makeBoard string
 | length string == 81 = Just (map makeBoard' (chunksOf 9 string))
 | otherwise           = Nothing
   where
     makeBoard' [] = []
     makeBoard' (x:xs)
       | x == '*' = [Possible [1..9]] ++ makeBoard' xs
       | otherwise = [Fixed (read [x] :: Int)] ++ makeBoard' xs


{- displayBoard board
Converts the board into graphical rows
-}
displayBoard :: Board -> String
displayBoard board = unlines (map displayBoard' (board))
  where
    displayBoard' [] = []
    displayBoard' (x:xs) =
      case x of
      Fixed x -> show x ++ " " ++ displayBoard' xs
      _ -> "* " ++ displayBoard' xs

{- checkRow row val
Removes val from every other cell in row
-}
checkRows :: Board -> Board
checkRows [] = []
checkRows board@(x:xs) = [checkRow x fixedCells] ++ checkRows xs
  where fixedCells = [x | Fixed x <- x]

--transpose (map (\x -> checkRow x cellVal) (transpose (map (\x -> checkRow x cellVal) board)))

checkRow :: Row -> [Int] -> Row
checkRow [] fixedCells = []
checkRow row@(x:xs) fixedCells =
  case x of
    Possible cell -> [Possible (cell \\ fixedCells)] ++ checkRow xs fixedCells
    _             -> [x] ++ checkRow xs fixedCells
  

{- checkSquare board val
Removes val from every other cell in the 3x3 square corresponding to val
-}
checkSquare :: Board -> Cell -> Board
checkSquare board val = undefined


test1 = TestCase $ assertEqual "Display board" ("* * * * * * * 1 * \n4 * * * * * * * * \n* 2 * * * * * * * \n* * * * 5 * 4 * 7 \n* * 8 * * * 3 * * \n* * 1 * 9 * * * * \n3 * * 4 * * 2 * * \n* 5 * 1 * * * * * \n* * * 8 * 6 * * * \n") (let Just board = makeBoard "*******1*4*********2***********5*4*7**8***3****1*9****3**4**2***5*1********8*6***" in displayBoard board)
test2 = TestCase $ assertEqual "Finished Board" True (let Just board = makeBoard "123456789123456789123456789123456789123456789123456789123456789123456789123456789" in finishedBoard board)

runtests = runTestTT $ TestList [test1, test2]                                 
