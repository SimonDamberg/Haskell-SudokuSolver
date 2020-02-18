import Data.List
import Data.List.Split
import Test.HUnit

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)

type Row = [Cell]

type Board = [Row]

{- finishedBoard board
Checks if all cells are filled with one value-}
finishedBoard :: Board -> Bool
finishedBoard board = undefined

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
checkRow :: Row -> Cell -> Row
checkRow [] cellVal = []
checkRow (x:xs) cellVal =
  case cellVal of
    Fixed val -> case x of
                   Possible cell -> [Possible (filter (val /=) cell)] ++ checkRow xs (Fixed val)
                   _ -> [x] ++ checkRow xs (Fixed val)
   
{- checkSquare board val
Removes val from every other cell in the 3x3 square corresponding to val
-}
checkSquare :: Board -> Cell -> Board
checkSquare board val = undefined

test1 = TestCase $ assertEqual "Display board" ("* * * * * * * 1 * \n4 * * * * * * * * \n* 2 * * * * * * * \n* * * * 5 * 4 * 7 \n* * 8 * * * 3 * * \n* * 1 * 9 * * * * \n3 * * 4 * * 2 * * \n* 5 * 1 * * * * * \n* * * 8 * 6 * * * \n") (let Just board = makeBoard "*******1*4*********2***********5*4*7**8***3****1*9****3**4**2***5*1********8*6***" in displayBoard board)

runtests = runTestTT $ TestList [test1]                                 
