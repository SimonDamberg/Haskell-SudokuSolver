module SudokuSolver where

import Data.List
import Data.List.Split
import Data.Function
import Control.Applicative
import Test.HUnit

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)

type Row = [Cell]

type Board = [Row]

{-solve board
Solves board-}
solve :: Board -> [(Board, Board)]-> Maybe[Board]
solve board acc = solve' (checkBoard board) acc
  where
    solve' board acc
      | possibleEmpty board = Nothing
      | finishedBoard board = Just [n |(a,b)<- acc ,n <-[a,b]]
      | otherwise           = let acc2 = newBoard board acc
                                  (board1, board2) = acc2 !! 0
                              in solve board1 acc2 <|> solve board2 acc2

{- newBoard board
Creates two possible boards from board
-}
newBoard :: Board -> [(Board, Board)] -> [(Board, Board)]
newBoard board acc = [(returnToBoard (replace i first board), returnToBoard (replace i rest board))] ++ acc
   where
    (i, first, rest) = fixCell (minimumBy (compare `on` (posCount . snd)) $ filter (cellPossible . snd) $ zip [0..80] $ concat board)
  
returnToBoard board = chunksOf 9 $ map snd board

replace i val board = let (first,last) = splitAt i (zip [0..80] $ concat board) in first ++ [(i,val)] ++ (tail last)

cellPossible (Possible _) = True
cellPossible _            = False

posCount (Possible xs) = length xs
posCount (Fixed _)     = 1

fixCell (i, Possible [x, y]) = (i, Fixed x, Fixed y)
fixCell (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
fixCell _                    = error "Impossible case"


{- finishedBoard board
Checks if all cells are filled with one value
-}
finishedBoard :: Board -> Bool
finishedBoard board = (length (foldl (++) "" (map finishedBoard' board))) == 81
  where
    finishedBoard' [] = []
    finishedBoard' (x:xs) =
      case x of
        Fixed x -> "0" ++ finishedBoard' xs
        _ -> "" ++ finishedBoard' xs

{- makeBoard string
Creates a board with all possible values in cells from a string
-}
makeBoard :: String -> Board
makeBoard string
 | length string == 81 = (map makeBoard' (chunksOf 9 string))
   where
     makeBoard' [] = []
     makeBoard' (x:xs)
       | x == '*' = [Possible [1..9]] ++ makeBoard' xs
       | otherwise = [Fixed (read [x] :: Int)] ++ makeBoard' xs


{- displayBoard board
Converts the board into graphical rows
-}
displayBoard :: Board -> String
displayBoard board = unlines (map displayBoard' board)
  where
    displayBoard' [] = []
    displayBoard' (x:xs) =
      case x of
      Fixed x -> show x ++ " " ++ displayBoard' xs
      _ -> "* " ++ displayBoard' xs

{- checkRows Board
Removes all fixed 
-}
checkRows :: Board -> Board
checkRows [] = []
checkRows board@(x:xs) = [checkRow x fixedCells] ++ checkRows xs
  where fixedCells = [x | Fixed x <- x]

checkRow :: Row -> [Int] -> Row
checkRow [] fixedCells = []
checkRow row@(x:xs) fixedCells =
  case x of
    Possible cell -> [Possible (cell \\ fixedCells)] ++ checkRow xs fixedCells
    _             -> [x] ++ checkRow xs fixedCells
  

{- checkBoard board 
Removes all Fixed cells from the 3x3 square, row and coloumn corresponding to the fixed cell
-}   
checkBoard :: Board -> Board
checkBoard board  = transpose $ checkRows $ transpose $ checkRows $ makeSquare $ checkRows $ makeSquare board

makeSquare :: Board -> Board
makeSquare [] = []
makeSquare board = makeSquare' ((map (chunksOf 3) board))
  where 
    makeSquare' [] = []
    makeSquare' (a:b:c:xs) = if null a then makeSquare' xs else [(concat square)] ++ (makeSquare' newBoard)
      where square = (map head [a, b, c])
            newBoard = (drop 1 a):(drop 1 b):(drop 1 c):xs

possibleEmpty :: Board -> Bool
possibleEmpty board = length (foldl (++) "" (map possibleEmpty' board)) > 0
  where
    possibleEmpty' [] = []
    possibleEmpty' (x:xs) =
      case x of
        Possible x -> if x == [] then "0"  else "" ++ possibleEmpty' xs
        _ -> "" ++ possibleEmpty' xs

{-
test1 = TestCase $ assertEqual "Display board" ("* * * * * * * 1 * \n4 * * * * * * * * \n* 2 * * * * * * * \n* * * * 5 * 4 * 7 \n* * 8 * * * 3 * * \n* * 1 * 9 * * * * \n3 * * 4 * * 2 * * \n* 5 * 1 * * * * * \n* * * 8 * 6 * * * \n") (let board = makeBoard "*******1*4*********2***********5*4*7**8***3****1*9****3**4**2***5*1********8*6***" in displayBoard board)
test2 = TestCase $ assertEqual "Finished Board" True (let board = makeBoard "123456789123456789123456789123456789123456789123456789123456789123456789123456789" in finishedBoard board)

runtests = runTestTT $ TestList [test1, test2]                                 
-}
