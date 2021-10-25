# Instructions
To be able to run the program, see the instructions below. If the Glasgow
Haskell Compiler and the required packages are already installed, you should
be able to skip to step 3.
## Packages
1. Install the Glasgow Haskell Compiler available [here](https://www.haskell.org/ghc/download.html).
2. Run the following command to install the required packages with cabal:
`cabal install gloss random List HUnit split`
3. Navigate to the folder containing the files in the terminal and run:
`ghc –make SudokuGloss.hs`
4. Run SudokuGloss.exe
## Controls
When the program is started, the user will be greeted with a random unsolved
Sudoku. Use the following controls to use the program.
* Esc: Quits the program
* R: Randomizes the board into a new Sudoku
* Space: Solves the Sudoku
