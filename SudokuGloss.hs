module Rendering where

import Graphics.Gloss

import Solver

boardAsUnsolved board = Blank

boardAsSolved solved board = Blank

startValues :: Board -> Picture
startValues = undefined

solvedValues :: Board -> Picture
solvedValues = undefined

boardGrid :: Board -> Picture

boardAsPicture board =
  pictures []

solverAsPicture :: Solver -> Picture
solverAsPicture solver = undefined