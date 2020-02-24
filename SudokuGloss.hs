module Rendering where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import SudokuSolver



window :: Display
window = InWindow "Sudoku Solver" (640, 480) (100, 100)

backgroundColor = white


gridBoard :: Picture
gridBoard =
  pictures
  $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                            , (i * cellWidth, fromIntegral screenHeight)
                            ]
                     , line [ (0.0,                      i * cellHeight)
                            , (fromIntegral screenWidth, i * cellHeight)
                            ]
                     ])
  [0.0..fromIntegral n]

main :: IO ()
main = backgroundColor 30 initialSolver boardAsPicture transfromSolver (const id)

{-

boardAsUnsolved board = Blank

boardAsSolved solved board = Blank

startValues :: Board -> Picture
startValues = undefined

solvedValues :: Board -> Picture
solvedValues = undefined

boardGrid :: Board -> Picture

boardAsPicture board =
  pictures []
-}


boardAsSolvingPicture board =

boardAsPicture board = Blank

snapPictureToCell picture (roq, column) = translate x y picture
  where x = fromIntegral column * cellWidth + cellWidth * 0.5
        y = fromIntegral row * cellHeight + cellHeight * 0.5

possibleCell :: Picture
possibleCell = Blank

fixedCell :: Picture
fixdCell = Blank


cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
  pictures
  $ map (snapPictureToCell cellPicture . fst)
  $ filter (\(_,e) -> e == cell  
  $ assoc board

possibleCellsOfBoard :: Board -> Picture
possibleCellsOfBoard = cellsOfBoard board (Possible [Int]) possibleCell


fixedCellsOfBoard :: Board -> Picture
fixedCellsOfBoard = cellsOfBoard board (Fixed Int) fixedCell



boardAsSolvedPicture solved = color  (boardAsPicture board)

solverAsPicture :: SudokuSolver -> Picture
solverAsPicture solve =
  case solverBoard solve of
    Unsolved -> boardAsSolvingPicture (solverBoard solve)
    Solved board -> boardAsSolvedPicture board (gameBoard solve)


