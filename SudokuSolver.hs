module SudokuSolver where

import Data.List
import Data.List.Split
import Data.Function
import Control.Applicative
import Test.HUnit

 {- A cell in a sudoku board
     Either a Fixed number or a list of Possible numbers
     INVARIANT: 0 < number < 10
   -}
data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)

 {- A row in a sudoku board
     A list of Cells
     INVARIANT: length == 9
   -}
type Row = [Cell]

 {- A sudoku board
     A list of Rows
     INVARIANT: length == 9 
   -}
type Board = [Row]

{- solve board acc
     Solves a sudoku
     PRE: length board == 81
     RETURNS: (the solved board):[all steps leading up to the solution]
     EXAMPLES: see solve.txt in project folder
  -}
solve :: Board -> [(Board, Board)]-> Maybe [Board] 
solve board acc = solve' (checkBoard board) acc
  where
    -- VARIANT: length $ filter cellPossible board
    solve' board acc -- checks if board is finished or is no longer valid
      | invalidBoard board = Nothing
      | finishedBoard board = Just [n |(a,b) <- acc, n <- [a,b]] -- concat all tupels in acc to become a single list of all the steps leading up to the solution
      | otherwise           = let acc2 = newBoard board acc
                                  (board1, board2) = acc2 !! 0
                              in solve board1 acc2 <|> solve board2 acc2

{- newBoard board acc
     finds the cell with the least amount of possible numbers and fixes 
     PRE: length board == 81
     RETURNS: (board with a cell fixed to a num, board with a cell fixed to another num or fewer possible nums):acc
     EXAMPLES: see newBoard.txt in project folder
  -}
newBoard :: Board -> [(Board, Board)] -> [(Board, Board)]
newBoard board acc = [(returnToBoard (replace i first board), returnToBoard (replace i rest board))] ++ acc
   where
    (i, first, rest) = fixCell (minimumBy (compare `on` (posCount . snd)) $ filter (cellPossible . snd) $ zip [0..80] $ concat board)

    -- turns a board with position i into a regular board 
    returnToBoard board = chunksOf 9 $ map snd board

    -- replaces the cell at position i with val 
    replace i val board = let (first,last) = splitAt i (zip [0..80] $ concat board) in first ++ [(i,val)] ++ (tail last)

    -- checks if a cell is fixed or not
    cellPossible (Possible _) = True
    cellPossible _            = False

    -- checks the number of possible numbers in a cell
    posCount (Possible xs) = length xs
    posCount (Fixed _)     = 1

    -- fixes a possible cell at position i in two possible ways 
    fixCell (i, Possible [x, y]) = (i, Fixed x, Fixed y)
    fixCell (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
    
{- finishedBoard board
     Checks if a board is complete
     PRE: length board == 81
     RETURNS: True if board has no possible cells, otherwise False
     EXAMPLES: finishedBoard  (makeBoard "*******12**36**********7***41**2*******5**3**7*****6**28*****4****3**5***********") = False
               finishedBoard  (makeBoard "123456789123456789123456789123456789123456789123456789123456789123456789123456789") = True
  -}
finishedBoard :: Board -> Bool
finishedBoard board = (length (foldl (++) "" (map finishedBoard' board))) == 81
  where
    -- VARIANT: length board
    finishedBoard' [] = []
    finishedBoard' (x:xs) =
      case x of
        Fixed x -> "0" ++ finishedBoard' xs
        _ -> "" ++ finishedBoard' xs

{- invalidBoard board
     Checks if a board has cells with no possible numbers, and is thereby invalid
     PRE: length board == 81
     RETURNS: True if board is invalid, otherwise False
     EXAMPLES: invalidBoard [[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Possible []]] = True
               invalidBoard (makeBoard "12345678912345678912345678912345678912345678912345678912345678912345678912345678*") = False
  -}
invalidBoard :: Board -> Bool
invalidBoard board = length (foldl (++) "" (map invalidBoard' board)) > 0
  where
    -- VARIANT: length board
    invalidBoard' [] = []
    invalidBoard' (x:xs) =
      case x of
        Possible x -> if x == [] then "0"  else "" ++ invalidBoard' xs
        _ -> "" ++ invalidBoard' xs

{- makeBoard string
     Constructs a board from a string
     PRE: length string == 81, nonfixed cells labeled with '*'
     RETURNS: a board corresponding to string
     EXAMPLES: makeBoard "*******123******6*****4****9*****5*******1*7**2**********35*4****14**8***6*******" = [[Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],
                                                                                                                 Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 1,Fixed 2], ...]
  -}
makeBoard :: String -> Board
makeBoard string
 | length string == 81 = (map makeBoard' (chunksOf 9 string))
   where
     -- VARIANT: length board
     makeBoard' [] = []
     makeBoard' (x:xs) -- constructs every cell in a row
       | x == '*' = [Possible [1..9]] ++ makeBoard' xs
       | otherwise = [Fixed (read [x] :: Int)] ++ makeBoard' xs


{- displayBoard board
     Constructs a printable string from a board
     PRE: length board == 81
     RETURNS: the string corresponding to board with \n at the end of each row
     EXAMPLES: displayBoard $ makeBoard "*******123******6*****4****9*****5*******1*7**2**********35*4****14**8***6*******"
               = "* * * * * * * 1 2 \n3 * * * * * * 6 * \n* * * * 4 * * * * \n9 * * * * * 5 * * \n* * * * * 1 * 7 * \n* 2 * * * * * * * \n* * * 3 5 * 4 * * \n* * 1 4 * * 8 * * \n* 6 * * * * * * * \n"
  -}
displayBoard :: Board -> String
displayBoard board = unlines (map displayBoard' board)
  where
    -- VARIANT: length board
    displayBoard' [] = []
    displayBoard' (x:xs) = -- constructs a string of every cell in a row
      case x of
      Fixed x -> show x ++ " " ++ displayBoard' xs
      _ -> "* " ++ displayBoard' xs

{- checkRows board 
     Removes all fixed values from every possible cell in the row with fixed values
     PRE: length board == 81
     RETURNS: board with fixed values removed from possible cells in each row
     EXAMPLES: checkRows $ makeBoard "*******123******6*****4****9*****5*******1*7**2**********35*4****14**8***6*******" = [[Possible [3,4,5,6,7,8,9],Possible [3,4,5,6,7,8,9],Possible [3,4,5,6,7,8,9],Possible [3,4,5,6,7,8,9],
                                                                                                                             Possible [3,4,5,6,7,8,9],Possible [3,4,5,6,7,8,9],Possible [3,4,5,6,7,8,9],Fixed 1,Fixed 2], ...]
  -}
checkRows :: Board -> Board
checkRows [] = []
checkRows board@(x:xs) = [checkRow x fixedCells] ++ checkRows xs
  where
    fixedCells = [x | Fixed x <- x]

    -- VARIANT: length board
    checkRow [] fixedCells = []
    checkRow row@(x:xs) fixedCells = -- removes all fixed values from the possible cells in a Row
      case x of
        Possible cell -> [Possible (cell \\ fixedCells)] ++ checkRow xs fixedCells
        _             -> [x] ++ checkRow xs fixedCells
  
{- checkBoard board
     Removes all fixed values from possible cells in the 3x3 square, row and coloumn corresponding to a fixed cell
     PRE: length board == 81
     RETURNS: board with fixed values removed from possible cells
     EXAMPLES: checkBoard (makeBoard "12345678912345678912345678912345678912345678912345678912345678912345678912345678*") = [[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Possible []]]
  -}  
checkBoard :: Board -> Board
checkBoard board  = transpose $ checkRows $ transpose $ checkRows $ makeSquare $ checkRows $ makeSquare board -- calls checkRows on the rows, coloumns and 3x3 squares using transpose and makeSquare to convert from a regular Board
  where
    -- VARIANT: length board
    makeSquare [] = []
    makeSquare board = makeSquare' ((map (chunksOf 3) board)) -- converts a list of rows into a list of the correct 3x3 squares 
      where
        -- VARIANT: length board
        makeSquare' [] = []
        makeSquare' (a:b:c:xs) = if null a then makeSquare' xs else [(concat square)] ++ (makeSquare' newBoard) -- concats all 3x3 sqaures into a Board for checkRows to use
          where
            square = (map head [a, b, c]) -- takes the first three cells in three rows, which makes up the correct 3x3 square
            newBoard = (drop 1 a):(drop 1 b):(drop 1 c):xs
            
test1 = TestCase $ assertEqual "Display board" ("* * * * * * * 1 * \n4 * * * * * * * * \n* 2 * * * * * * * \n* * * * 5 * 4 * 7 \n* * 8 * * * 3 * * \n* * 1 * 9 * * * * \n3 * * 4 * * 2 * * \n* 5 * 1 * * * * * \n* * * 8 * 6 * * * \n") (let board = makeBoard "*******1*4*********2***********5*4*7**8***3****1*9****3**4**2***5*1********8*6***" in displayBoard board)

test2 = TestCase $ assertEqual "Finished Board" True (finishedBoard $ makeBoard "123456789123456789123456789123456789123456789123456789123456789123456789123456789")

test3 = TestCase $ assertEqual "Make Board" [[Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 1,Fixed 2],[Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 8,Possible [1,2,3,4,5,6,7,8,9],Fixed 3,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]],[Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 4,Possible [1,2,3,4,5,6,7,8,9]],[Fixed 1,Fixed 2,Possible [1,2,3,4,5,6,7,8,9],Fixed 5,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]],[Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 4,Fixed 7,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]],[Possible [1,2,3,4,5,6,7,8,9],Fixed 6,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]],[Fixed 5,Possible [1,2,3,4,5,6,7,8,9],Fixed 7,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 3,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]],[Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 6,Fixed 2,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]],[Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Fixed 1,Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9],Possible [1,2,3,4,5,6,7,8,9]]] (makeBoard "*******12**8*3***********4*12*5**********47***6*******5*7***3*****62*******1*****")

test4 = TestCase $ assertEqual "Check Rows" [[Possible [3,4,5,6,7,8,9],Possible [3,4,5,6,7,8,9],Possible [3,4,5,6,7,8,9],Possible [3,4,5,6,7,8,9],Possible [3,4,5,6,7,8,9],Possible [3,4,5,6,7,8,9],Possible [3,4,5,6,7,8,9],Fixed 1,Fixed 2],[Possible [1,2,4,5,6,7,9],Possible [1,2,4,5,6,7,9],Fixed 8,Possible [1,2,4,5,6,7,9],Fixed 3,Possible [1,2,4,5,6,7,9],Possible [1,2,4,5,6,7,9],Possible [1,2,4,5,6,7,9],Possible [1,2,4,5,6,7,9]],[Possible [1,2,3,5,6,7,8,9],Possible [1,2,3,5,6,7,8,9],Possible [1,2,3,5,6,7,8,9],Possible [1,2,3,5,6,7,8,9],Possible [1,2,3,5,6,7,8,9],Possible [1,2,3,5,6,7,8,9],Possible [1,2,3,5,6,7,8,9],Fixed 4,Possible [1,2,3,5,6,7,8,9]],[Fixed 1,Fixed 2,Possible [3,4,6,7,8,9],Fixed 5,Possible [3,4,6,7,8,9],Possible [3,4,6,7,8,9],Possible [3,4,6,7,8,9],Possible [3,4,6,7,8,9],Possible [3,4,6,7,8,9]],[Possible [1,2,3,5,6,8,9],Possible [1,2,3,5,6,8,9],Possible [1,2,3,5,6,8,9],Possible [1,2,3,5,6,8,9],Possible [1,2,3,5,6,8,9],Fixed 4,Fixed 7,Possible [1,2,3,5,6,8,9],Possible [1,2,3,5,6,8,9]],[Possible [1,2,3,4,5,7,8,9],Fixed 6,Possible [1,2,3,4,5,7,8,9],Possible [1,2,3,4,5,7,8,9],Possible [1,2,3,4,5,7,8,9],Possible [1,2,3,4,5,7,8,9],Possible [1,2,3,4,5,7,8,9],Possible [1,2,3,4,5,7,8,9],Possible [1,2,3,4,5,7,8,9]],[Fixed 5,Possible [1,2,4,6,8,9],Fixed 7,Possible [1,2,4,6,8,9],Possible [1,2,4,6,8,9],Possible [1,2,4,6,8,9],Fixed 3,Possible [1,2,4,6,8,9],Possible [1,2,4,6,8,9]],[Possible [1,3,4,5,7,8,9],Possible [1,3,4,5,7,8,9],Possible [1,3,4,5,7,8,9],Fixed 6,Fixed 2,Possible [1,3,4,5,7,8,9],Possible [1,3,4,5,7,8,9],Possible [1,3,4,5,7,8,9],Possible [1,3,4,5,7,8,9]],[Possible [2,3,4,5,6,7,8,9],Possible [2,3,4,5,6,7,8,9],Possible [2,3,4,5,6,7,8,9],Fixed 1,Possible [2,3,4,5,6,7,8,9],Possible [2,3,4,5,6,7,8,9],Possible [2,3,4,5,6,7,8,9],Possible [2,3,4,5,6,7,8,9],Possible [2,3,4,5,6,7,8,9]]] (checkRows $ makeBoard "*******12**8*3***********4*12*5**********47***6*******5*7***3*****62*******1*****")

test5 = TestCase $ assertEqual "Invalid Board 1" False (invalidBoard (makeBoard "12345678912345678912345678912345678912345678912345678912345678912345678912345678*"))

test6 = TestCase $ assertEqual "Invalid Board 2" True (invalidBoard [[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Possible []]])

test7 = TestCase $ assertEqual "Check Board" [[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Fixed 9],[Fixed 1,Fixed 2,Fixed 3,Fixed 4,Fixed 5,Fixed 6,Fixed 7,Fixed 8,Possible []]] (checkBoard (makeBoard "12345678912345678912345678912345678912345678912345678912345678912345678912345678*"))

test8 = TestCase $ assertEqual "Solve" [[Possible [3,5,7,8,9],Possible [3,5,6,8,9],Possible [3,5,6,7,8,9],Possible [3,4,7,8],Possible [3,4,6,7],Possible [3,4,6,8],Possible [6,7,8,9],Fixed 1,Fixed 2],[Possible [1,3,7,8,9],Fixed 4,Possible [2,3,6,7,8,9],Possible [2,3,7,8],Fixed 5,Possible [1,2,3,6,8],Possible [6,7,8,9],Possible [3,6,7,8,9],Possible [3,6,7,8,9]],[Possible [1,3,5,7,8],Possible [1,2,3,5,6,8],Possible [2,3,5,6,7,8],Possible [2,3,4,7,8],Possible [1,2,3,4,6,7],Fixed 9,Possible [6,7,8],Possible [3,4,6,7,8],Possible [3,4,5,6,7,8]],[Possible [1,3,5,8,9],Fixed 7,Possible [2,3,5,8,9],Fixed 6,Possible [2,3,9],Possible [2,3,5,8],Fixed 4,Possible [2,3,8,9],Possible [1,3,8,9]],[Possible [3,4,5,8,9],Possible [2,3,5,6,8,9],Possible [2,3,4,5,6,8,9],Fixed 1,Possible [2,3,4,7,9],Possible [2,3,4,5,8],Possible [2,6,7,8,9],Possible [2,3,6,7,8,9],Possible [3,6,7,8,9]],[Possible [1,3,4,8,9],Possible [1,2,3,6,8,9],Possible [2,3,4,6,8,9],Possible [2,3,4,7,8,9],Possible [2,3,4,7,9],Possible [2,3,4,8],Possible [1,2,6,7,8,9],Fixed 5,Possible [1,3,6,7,8,9]],[Possible [3,4,9],Fixed 9,Possible [3,4,9],Possible [2,3,4,9],Fixed 8,Fixed 7,Fixed 5,Possible [2,4,6,9],Possible [1,4,6,9]],[Fixed 6,Possible [5,8,9],Fixed 1,Possible [2,4,5,9],Possible [2,4,9],Possible [2,4,5],Fixed 3,Possible [2,4,7,8,9],Possible [4,7,8,9]],[Fixed 2,Possible [3,5,8,9],Possible [3,4,5,7,8,9],Possible [3,4,5,9],Possible [1,3,4,6,9],Possible [1,3,4,5,6],Possible [1,6,7,8,9],Possible [4,6,7,8,9],Possible [1,4,6,7,8,9]]] (let Just solved = solve (makeBoard "*******12*4**5*********9****7*6**4*****1************5*****875**6*1***3**2********") [] in last solved)

test9 = TestCase $ assertEqual "newBoard" [([[Possible [5,6,8,9],Possible [4,5,6,7,9],Possible [4,5,6,7,8,9],Fixed 4,Possible [3,4,5,8,9],Possible [3,4,5,8,9],Possible [4,7,8,9],Fixed 1,Fixed 2],[Possible [1,5,8,9],Possible [2,4,5,7,9],Fixed 3,Fixed 6,Possible [1,4,5,8,9],Possible [1,2,4,5,8,9],Possible [4,7,8,9],Possible [5,7,8,9],Possible [4,5,7,8,9]],[Possible [1,5,6,8,9],Possible [2,4,5,6,9],Possible [1,2,4,5,6,8,9],Possible [1,2,4,8,9],Possible [1,3,4,5,8,9],Fixed 7,Possible [4,8,9],Possible [3,5,6,8,9],Possible [3,4,5,6,8,9]],[Fixed 4,Fixed 1,Possible [5,6,8,9],Possible [7,8,9],Fixed 2,Possible [3,6,8,9],Possible [7,8,9],Possible [5,7,8,9],Possible [5,7,8,9]],[Possible [6,8,9],Possible [2,6,9],Possible [2,6,8,9],Fixed 5,Possible [1,4,6,7,8,9],Possible [1,4,6,8,9],Fixed 3,Possible [2,7,8,9],Possible [1,4,7,8,9]],[Fixed 7,Possible [2,3,5,9],Possible [2,5,8,9],Possible [1,4,8,9],Possible [1,3,4,8,9],Possible [1,3,4,8,9],Fixed 6,Possible [2,5,8,9],Possible [1,4,5,8,9]],[Fixed 2,Fixed 8,Possible [1,5,6,7,9],Possible [1,7,9],Possible [1,5,6,7,9],Possible [1,5,6,9],Possible [1,7,9],Fixed 4,Possible [1,3,6,7,9]],[Possible [1,6,9],Possible [4,6,7,9],Possible [1,4,6,7,9],Fixed 3,Possible [1,4,6,7,8,9],Possible [1,2,4,6,8,9],Fixed 5,Possible [2,6,7,8,9],Possible [1,6,7,8,9]],[Possible [1,3,5,6,9],Possible [3,4,5,6,7,9],Possible [1,4,5,6,7,9],Possible [1,2,4,7,8,9],Possible [1,4,5,6,7,8,9],Possible [1,2,4,5,6,8,9],Possible [1,2,7,8,9],Possible [2,3,6,7,8,9],Possible [1,3,6,7,8,9]]],[[Possible [5,6,8,9],Possible [4,5,6,7,9],Possible [4,5,6,7,8,9],Possible [8,9],Possible [3,4,5,8,9],Possible [3,4,5,8,9],Possible [4,7,8,9],Fixed 1,Fixed 2],[Possible [1,5,8,9],Possible [2,4,5,7,9],Fixed 3,Fixed 6,Possible [1,4,5,8,9],Possible [1,2,4,5,8,9],Possible [4,7,8,9],Possible [5,7,8,9],Possible [4,5,7,8,9]],[Possible [1,5,6,8,9],Possible [2,4,5,6,9],Possible [1,2,4,5,6,8,9],Possible [1,2,4,8,9],Possible [1,3,4,5,8,9],Fixed 7,Possible [4,8,9],Possible [3,5,6,8,9],Possible [3,4,5,6,8,9]],[Fixed 4,Fixed 1,Possible [5,6,8,9],Possible [7,8,9],Fixed 2,Possible [3,6,8,9],Possible [7,8,9],Possible [5,7,8,9],Possible [5,7,8,9]],[Possible [6,8,9],Possible [2,6,9],Possible [2,6,8,9],Fixed 5,Possible [1,4,6,7,8,9],Possible [1,4,6,8,9],Fixed 3,Possible [2,7,8,9],Possible [1,4,7,8,9]],[Fixed 7,Possible [2,3,5,9],Possible [2,5,8,9],Possible [1,4,8,9],Possible [1,3,4,8,9],Possible [1,3,4,8,9],Fixed 6,Possible [2,5,8,9],Possible [1,4,5,8,9]],[Fixed 2,Fixed 8,Possible [1,5,6,7,9],Possible [1,7,9],Possible [1,5,6,7,9],Possible [1,5,6,9],Possible [1,7,9],Fixed 4,Possible [1,3,6,7,9]],[Possible [1,6,9],Possible [4,6,7,9],Possible [1,4,6,7,9],Fixed 3,Possible [1,4,6,7,8,9],Possible [1,2,4,6,8,9],Fixed 5,Possible [2,6,7,8,9],Possible [1,6,7,8,9]],[Possible [1,3,5,6,9],Possible [3,4,5,6,7,9],Possible [1,4,5,6,7,9],Possible [1,2,4,7,8,9],Possible [1,4,5,6,7,8,9],Possible [1,2,4,5,6,8,9],Possible [1,2,7,8,9],Possible [2,3,6,7,8,9],Possible [1,3,6,7,8,9]]])] (newBoard (checkBoard (makeBoard "*******12**36**********7***41**2*******5**3**7*****6**28*****4****3**5***********")) [])

runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9]                                  

