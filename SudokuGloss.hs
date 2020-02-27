import System.Environment
import System.Random
import System.Exit
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace
import SudokuSolver
import Control.Concurrent

cellWidth :: Float
cellWidth = fromIntegral 720 / fromIntegral 9

cellHeight :: Float
cellHeight = fromIntegral 720 / fromIntegral 9

fixedColor = makeColorI 53 152 55 255

main :: IO ()
main = do
  board <- fixNewBoard
  playIO FullScreen white 6 board displayBoardOnGrid eventBoard floatBoard

fixNewBoard = do
  boardFile <- readFile "sudoku17.txt"
  let boards = lines boardFile
  randInt <- randomRIO (1, 49000) :: IO Int
  board <- (randSudokuBoard randInt boards) 
  return [board]

randSudokuBoard x boardList = return $ makeBoard $ head $ drop x boardList

eventBoard :: Event -> [Board] -> IO [Board]  
eventBoard event board = case event of
  EventKey (SpecialKey KeySpace) Down _ _ -> case solve (head board) [] of
                    Just solved -> return solved           
  EventKey (Char 'r') Down _ _ -> fixNewBoard
  EventKey (SpecialKey KeyEsc) _ _ _ -> exitSuccess
  _ -> return board

floatBoard :: Float -> [Board] -> IO [Board]
floatBoard float board
  | length board <= 1 = return board
  | otherwise = return $ init board
    
displayBoardOnGrid :: [Board] -> IO Picture
displayBoardOnGrid board = return (translate (fromIntegral 720 * (-0.5)) (fromIntegral 720 * (-0.5)) (pictures ((displayBoardOnGrid' (zip [0..80] $ concat $ last board)) ++ [gridBoard] ++ [thickBoard])))

displayBoardOnGrid' :: [(Int, Cell)] -> [Picture]
displayBoardOnGrid' [] = []
displayBoardOnGrid' (x:xs) = [displayCell x] ++  displayBoardOnGrid' xs

displayCell (i, val) =
  case val of
    Fixed num -> translate (((fromIntegral (i `mod` 9) :: Float) * cellWidth) + 17) (((fromIntegral (8 - (i `div` 9)) :: Float) * cellHeight) + 9.5) (color fixedColor $ scale 0.60 0.60 $ text $ show num)
    _         -> Blank


    
gridBoard :: Picture
gridBoard = pictures $ concatMap (\i -> [line [(i * cellWidth, 0.0) ,(i * cellWidth, fromIntegral 720)], line [(0.0, i * cellHeight), (fromIntegral 720, i * cellHeight)]]) [0.0..fromIntegral 9]


thickBoard :: Picture
thickBoard = pictures $ concatMap (\i -> [line [(i * cellWidth, 0.0) ,(i * cellWidth, fromIntegral 720)], line [(0.0, i * cellHeight), (fromIntegral 720, i * cellHeight)]]) [0.01, 3.01, 6.01, 2.99, 5.99, 8.98, 8.99]

