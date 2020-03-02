import System.Environment
import System.Random
import System.Exit
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import SudokuSolver

-- Calculates the width of a cell and returns it as a float
cellWidth :: Float
cellWidth = fromIntegral 720 / fromIntegral 9

-- Calculates the height of a cell and returns it as a float
cellHeight :: Float
cellHeight = fromIntegral 720 / fromIntegral 9

-- Makes a color with RGBA values
fixedColor = makeColorI 53 152 55 255

{- main
   Runs the game using the playIO function
-}
main :: IO ()
main = do
  board <- fixNewBoard
  playIO FullScreen white 6 board displayBoardOnGrid eventBoard floatBoard

{-fixNewBoard
  Reads a file with 49000 sudokus, and randomly chooses one of these and returns it as a list
-}
fixNewBoard = do
  boardFile <- readFile "sudoku.txt"
  let boards = lines boardFile
  randInt <- randomRIO (1, 49000) :: IO Int
  board <- (randSudokuBoard randInt boards) 
  return [board]
    where
      randSudokuBoard x boardList = return $ makeBoard $ head $ drop x boardList

{- eventBoard event startList
   Does specific actions when some key is pressed. If the key is space, it solves the board, if r then a new board is displayed, if esc it exits the program. Otherwise it just returns the input
   RETURNS: If space, then it returns a list of all steps when solving startList. If r, then it returns a new single board. If esc, then it returns nothing and exits the program. If none of these keys are pressed, startList is returned.
   EXAMPLES: Impossible to do examples
-}
eventBoard :: Event -> [Board] -> IO [Board]  
eventBoard event board = case event of
  EventKey (SpecialKey KeySpace) Down _ _ -> case solve (head board) [] of
                    Just solved -> return solved           
  EventKey (Char 'r') Down _ _ -> fixNewBoard
  EventKey (SpecialKey KeyEsc) _ _ _ -> exitSuccess
  _ -> return board
  
{- floatBoard float startList
   Updates the list of boards every tick of the playIO-loop
   RETURNS: Returns a list which is everything but the last element of startList. If startList only has one element, then startList is returned.
   EXAMPLES: Impossible to do examples
-}

floatBoard :: Float -> [Board] -> IO [Board]
floatBoard float board
  | length board <= 1 = return board
  | otherwise = return $ init board

{- displayBoardOnGrid boardList
   Displays the last element in boardList as a picture at a specific position. Also displays the grid and the extra thick grid on top of this.
   RETURNS: A picture which consists of the last element in boardList and a grid.
   EXAMPLE: Impossible to do examples
-}
displayBoardOnGrid :: [Board] -> IO Picture
displayBoardOnGrid board = return (translate (fromIntegral 720 * (-0.5)) (fromIntegral 720 * (-0.5)) (pictures ((map displayCell (zip [0..80] $ concat $ last board)) ++ [gridBoard] ++ [thickBoard])))

{- displayCell (pos, value)
   Displays all the cells of the board. If a cell is fixed, then that number is written out and put in the position according to its number in the tuple. If a cell isn't fixed then a blank square is written out at the right position.
   RETURNS: A picture of value at a position according to its pos
   EXAMPLES: Impossible to do examples
-}
displayCell :: (Int, Cell) -> Picture
displayCell (i, val) =
  case val of
    Fixed num -> translate (((fromIntegral (i `mod` 9) :: Float) * cellWidth) + 17) (((fromIntegral (8 - (i `div` 9)) :: Float) * cellHeight) + 9.5) (color fixedColor $ scale 0.60 0.60 $ text $ show num)
    _         -> Blank

{- gridBoard
   gridBoard makes a picture with a 9x9 grid
   RETURNS: A picture with 9x9 lines painted
   EXAMPLES: Impossible to do examples
-}
gridBoard :: Picture
gridBoard = pictures $ concatMap (\i -> [line [(i * cellWidth, 0.0) ,(i * cellWidth, fromIntegral 720)], line [(0.0, i * cellHeight), (fromIntegral 720, i * cellHeight)]]) [0.0..fromIntegral 9]

{- thickBoard
   thickBoard makes a picture with extra thick lines every third line
   RETURNS: A picture with thicker lines at line 1, 3, 6, 9 both horizontal and vertical
   EXAMPLES: Impossible to do examples
-}
thickBoard :: Picture
thickBoard = pictures $ concatMap (\i -> [line [(i * cellWidth, 0.0) ,(i * cellWidth, fromIntegral 720)], line [(0.0, i * cellHeight), (fromIntegral 720, i * cellHeight)]]) [0.01, 3.01, 6.01, 2.99, 5.99, 8.98, 8.99]
