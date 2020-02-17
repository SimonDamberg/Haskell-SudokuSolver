import Data.List
import Data.List.Split

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
checkRow row val = undefined

{- checkSquare board val
Removes val from every other cell in the 3x3 square corresponding to val
-}
checkSquare :: Board -> Cell -> Board
checkSquare board val = undefined

 {-
test1 = TestCase $ assertEqual "makeBoard" ([Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 1,Possible [1,2,3,4,5,6,7,8,9]]
[Fixed 4,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]]
[Possible [1,2,3,4,5,6,7,8,9],Fixed 2,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]]
[Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 5,Possible [1,2,3,4,5,6,7,8,9],Fixed 4,Possible [1,2,3,4,5,6,7,8,9],Fixed 7]
[Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 8,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 3,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]]
[Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 1,Possible [1,2,3,4,5,6,7,8,9],Fixed 9,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]]
[Fixed 3,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 4,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 2,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]]
[Possible [1,2,3,4,5,6,7,8,9],Fixed 5,Possible [1,2,3,4,5,6,7,8,9],Fixed 1,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]]
[Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 8,Possible [1,2,3,4,5,6,7,8,9],Fixed 6,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]]) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]
-}
