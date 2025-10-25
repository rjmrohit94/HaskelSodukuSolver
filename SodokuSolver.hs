-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where
import Sudoku(Board, Solutions(..))
import Data.List (elemIndex)
import Debug.Trace (trace)

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 
    Implementation Description:
    This implementation uses a backtracking algorithm to count the number of possible solutions for a given Sudoku board. The Sudoku board is a list of lists, where each list represents a row in the grid. Empty cells are represented by 0, and we attempt to fill these cells with numbers that respect the Sudoku rules: no duplicate numbers in the same row, column, or subgrid.

    The function `numSolutions` returns whether the Sudoku has 0, 1, or more than 1 solutions. We use a recursive function to try placing numbers in empty cells, and if a valid placement is found, the function continues recursively. If the board is fully filled in a valid way, we count it as a solution. If more than one solution is found, we return `MultipleSolutions`. If none is found, we return `NoSolution`, and if exactly one solution is found, we return `UniqueSolution`.
-}

author :: String
author = "Rohit Joseph Mamutil"  -- replace with your full name

nickname :: String
nickname = "RJM"  -- replace with a nickname of your choice

{- numSolutions board
   Counts the number of possible solutions for a Sudoku board.
   PRE: The board is a square grid.  
        empty cells represented by 0.
   RETURNS: NoSolution if there are no solutions,
            UniqueSolution if exactly one solution exists,
            MultipleSolutions if multiple.
-}
numSolutions :: Board -> Solutions
numSolutions board = case countSolutions board 0 of
    0 -> NoSolution
    1 -> UniqueSolution
    _ -> MultipleSolutions

{- findEmpty board
   Finds the first empty cell in the board.
   PRE: The board is a square grid.
   RETURNS: A tuple (row, col) of the coordinates of the first empty cell, or Nothing if there are no empty cells.
   EXAMPLES:
       findEmpty [[1,2,0],[4,5,6],[7,8,9]] == Just (0, 2)
       findEmpty [[1,2,3],[4,5,6],[7,8,9]] == Nothing
-}
findEmpty :: Board -> Maybe (Int, Int)
findEmpty board = findEmptyInRow board 0
  where
    findEmptyInRow [] _ = Nothing --no more rows to check
    findEmptyInRow (row:rows) r = -- take one row... use pattern matching?- yeah
      case elemIndex 0 row of -- we need the empty cell, find the column
        Just c  -> Just (r, c)
        Nothing -> findEmptyInRow rows (r + 1) -- move to the next row







-- Helper function: Check if placing a number in a specific position is valid
-- we need to check 3 things,
-- is the number in the row?
-- is the number in the column?
-- is the number in the sub grid(the n*n section of the board)?




{- isValid board num (r, c)
   Checks if placing num at position (r, c) is valid.
   PRE: The board is a square grid, and (r, c) is within the board's bounds.
   RETURNS: True if placing num in the specified position satisfies Sudoku rules; otherwise, False.
   EXAMPLES:
       isValid [[0,2,3],[4,5,6],[7,8,9]] 1 (0, 0) == True
       isValid [[1,2,3],[4,5,6],[7,8,9]] 1 (0, 1) == False
-}
isValid :: Board -> Int -> (Int, Int) -> Bool
isValid board num (r, c) = 
    not (num `elem` (getRow r board)) &&  -- Check row num elem checks if that is an element of that row
    not (num `elem` (getCol c board)) &&  -- Check column
    not (num `elem` (getBox r c board))   -- Check box

{- getRow r board
   finds the row at index r from the board.
   RETURNS: A list of integers representing the row at index r.
-}
getRow :: Int -> Board -> [Int]
getRow r board = board !! r -- !! operator to get index, so here it gets the row at r th position

{- getCol c board
   Retrieves the column at index c from the board.
   RETURNS: A list of integers representing the column at index c.
-}
getCol :: Int -> Board -> [Int]
getCol c board = map (!! c) board -- similar to above but now we need column, (!! c) is applied to each row.

{- getBox r c board
   Retrieves the subgrid that contains the cell at (r, c).
   RETURNS: A list of integers representing the numbers within the subgrid.
-}
getBox :: Int -> Int -> Board -> [Int]
getBox r c board =
  let n = length board
      sqrtN = floor . sqrt . fromIntegral $ n  -- Calculate the subgrid size dynamically
      br = (r `div` sqrtN) * sqrtN  -- Starting row of the subgrid
      bc = (c `div` sqrtN) * sqrtN  -- Starting column of the subgrid
  in [ board !! (br + i) !! (bc + j) | i <- [0..(sqrtN-1)], j <- [0..(sqrtN-1)] ]


{- countSolutions board count
   Recursively counts the number of possible solutions by attempting to fill empty cells.
   PRE: The board is a square grid, and count is non-negative.
   RETURNS: The number of solutions found if the count does not exceed 1; otherwise, returns the existing count.
   SIDE EFFECTS: It could take a long time when there are many empty elements or when board size gets bigger
-}

--Variant I am not sure here, correct me if i am wrong, i could not find a variant here so added side effect. I could think of following
-- 'findEmpty board' -> this should eventually become Nothing hence terminating
countSolutions :: Board -> Int -> Int
countSolutions board count
    | count > 1 = count  -- Using the guards here, if count is more than 1 then it is multiple solutions
    | otherwise = case findEmpty board of
        Nothing ->{- trace ("No empty cells found. Found a solution! Current Board\n " ++ show (board))-} (count + 1)-- No empty cells, found a solution
        Just (r, c) ->  -- now, if there is an empty cell, try inserting a value,, what value?? [1-9]
            let n = length board  -- Get the size of the board
                newCount = foldl (\accumulatedCount num -> if isValid board num (r, c)
                    then {-trace ("New board\n"++show(board)) $-}countSolutions (placeNumber board num (r, c)) accumulatedCount
                    else accumulatedCount) count [1..n] -- Use [1..n] for the range
            in newCount

{- placeNumber board num (r, c)
   Places num on the board at the position (r, c).
   PRE: The board is a square grid, and (r, c) is within bounds.
   RETURNS: A new board with num placed at the specified position.
   EXAMPLES:
       placeNumber [[1,2,0],[4,5,6],[7,8,9]] 3 (0, 2) == [[1,2,3],[4,5,6],[7,8,9]]
-}
-- place a number on the board
placeNumber :: Board -> Int -> (Int, Int) -> Board
placeNumber board num (r, c) = -- board- the current state of board, num- the number to insert, (r,c)- tuple with row and column 
    take r board ++ [take c (board !! r) ++ [num] ++ drop (c + 1) (board !! r)] ++ drop (r + 1) board
-- take r board -> take rows above row r
-- board !! r -> take r th row from board
-- take c (board !! r) ->  take first c elements from r th row
-- [num] -> num in list form
-- drop (c + 1) (board !! r)




-- Following are sample boards I have tried out
sampleBoardTest :: Board
sampleBoardTest = [[0,0,3,4],
                   [4,0,2,1],
                   [3,0,1,2],
                   [2,1,4,3]]
sampleBoard1 :: Board
sampleBoard1 = [ 
                [0, 6, 8, 0, 0, 2, 0, 0, 0]
                , [0, 7, 0, 0, 0, 0, 0, 0, 0]
                , [0, 0, 0, 0, 0, 0, 0, 0, 0]
                , [0, 2, 0, 0, 0, 0, 0, 7, 8]
                , [0, 0, 0, 2, 0, 0, 5, 0, 0]
                , [4, 0, 5, 8, 7, 0, 0, 0, 0]
                , [0, 0, 0, 0, 0, 7, 0, 2, 0]
                , [8, 3, 7, 0, 0, 0, 4, 0, 0]
                , [2, 5, 0, 0, 0, 0, 0, 0, 0]
                ]
sampleBoard1UniqueTest :: Board
sampleBoard1UniqueTest = [ 
                [1, 6, 8, 3, 4, 2, 7, 5, 9]
                , [3, 7, 2, 1, 5, 9, 6, 8, 4]
                , [5, 4, 9, 7, 6, 8, 1, 3, 2]
                , [6, 2, 0, 0, 0, 0, 0, 7, 8]
                , [0, 0, 0, 2, 0, 0, 5, 0, 0]
                , [4, 0, 5, 8, 7, 0, 0, 0, 3]
                , [9, 0, 0, 0, 0, 7, 0, 2, 5]
                , [8, 3, 7, 0, 0, 0, 4, 9, 1]
                , [2, 5, 4, 9, 1, 3, 8, 6, 7]
                ]

sampleBoard1UnsolvableTest :: Board
sampleBoard1UnsolvableTest = [ 
                [5, 1, 6, 8, 4, 9, 7, 3, 2]
                , [3, 0, 7, 6, 0, 5, 0, 0, 0]
                , [8, 0, 9, 7, 0, 0, 0, 6, 5]
                , [1, 3, 5, 0, 6, 0, 9, 0, 7]
                , [4, 7, 2, 5, 9, 1, 0, 0, 6]
                , [9, 6, 8, 3, 7, 0, 0, 5, 0]
                , [2, 5, 3, 1, 8, 6, 0, 7, 4]
                , [6, 8, 4, 2, 0, 7, 5, 0, 0]
                , [7, 9, 1, 0, 5, 0, 6, 0, 8]
                ]

sampleBoard2 :: Board
sampleBoard2 = [ 
                  [0,0,0,  0,0,0,  0,0,5]
                , [5,9,0,  0,1,0,  0,0,0]
                , [0,2,0,  0,0,0,  7,0,0]
                , [2,0,4,  9,6,0,  0,5,0]
                , [0,0,5,  0,4,0,  0,0,0]
                , [0,0,0,  7,0,0,  0,0,0]
                , [0,0,0,  3,0,0,  0,0,0]
                , [3,4,0,  0,0,0,  0,0,1]
                , [8,0,0,  0,0,0,  4,0,3]
                ]
sampleBoard3Test4 ::Board
sampleBoard3Test4 =[
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7],
    [0,0,0,0,0,6,11,7,9,0,2,12,0,0,0,0],
    [0,0,0,13,0,0,0,0,0,14,0,11,0,0,0,0],
    [0,0,0,0,0,2,12,0,0,0,0,0,0,0,15,1],
    [0,0,0,0,0,0,0,2,0,0,1,0,0,11,0,0],
    [0,0,10,0,5,0,8,0,15,11,0,0,0,12,0,6],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,0],
    [0,0,0,0,12,9,0,0,0,13,0,4,0,0,1,0],
    [0,0,4,0,0,0,0,0,5,0,0,16,0,0,0,0],
    [0,13,8,0,1,0,0,5,0,0,0,0,0,0,0,0],
    [0,0,0,15,0,0,0,0,13,0,0,0,16,0,0,0],
    [0,5,0,0,0,0,15,0,0,0,0,6,0,0,0,0],
    [6,0,0,0,2,0,0,0,0,0,0,10,1,15,0,0],
    [0,0,0,0,15,0,0,0,14,0,0,0,0,0,13,4],
    [0,0,13,0,0,5,10,8,3,0,0,0,0,6,0,0],
    [0,3,11,1,0,0,0,0,0,0,0,0,10,0,5,8]]
sampleBoard7 :: Board
sampleBoard7 = [
    [0 ,  2,  0,  0, 23,  0,      0,  0,  0,  0, 32,  0,     16,  0,  0,  0, 19, 36,      0,  0, 18,  0,  0,  7,     10,  0,  0,  0,  0,  0,     30,  0,  0,  0,  0,  0],
    [0 , 28,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,     32,  0, 29, 21,  0,  0,      0, 35,  2, 25,  6,  0,     11, 36,  0, 15,  0,  0,      0,  0,  0,  0,  0, 22],
    [14,  0,  0,  0,  0,  0,      0, 26,  0,  0, 18,  0,      0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0],
    [9 , 32,  0, 31,  0,  0,      0,  4, 30,  0,  0,  1,      2, 35,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,      0, 22,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0],
    [13,  0, 26,  0,  7, 24,     15,  0,  0,  0,  0,  0,      8,  0,  0,  5,  0,  0,      0,  0,  0,  0,  0,  0,      0,  0,  0,  3,  0,  9,      0,  0,  0,  0,  0,  0],
    [0 ,  0,  0,  0,  0, 15,     35, 34,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,      0,  0,  0, 33, 14,  0,      0,  1,  0,  0,  0,  0,      0,  3,  0, 29,  0,  0],

    [0 ,  0,  0,  0,  0,  0,      0,  0,  0, 10,  0,  0,      0,  0,  0,  0,  0,  0,      9, 0, 25, 0, 0, 0, 0, 0, 11, 2, 0, 0, 0, 0, 0, 19, 26, 0],
    [0 ,  0,  0, 24, 26,  0,      0,  6,  0,  0,  0,  0,      0,  0,  0,  0,  5,  0,      0, 0, 1, 0, 0, 0, 30, 0, 0, 28, 0, 0, 23, 0, 0, 0, 0, 0],
    [0 ,  0,  0,  0, 10, 18,      0,  0,  7,  0, 22,  0,      0,  0,  0,  0, 12,  0,      30, 0, 31, 0, 0, 0, 0, 0, 0, 32, 0, 23, 0, 2, 0, 0, 0, 0],
    [0 ,  0,  6,  0,  0,  2,      0,  0, 23,  0,  0,  0,      0,  0,  0, 26,  0,  0,      13, 0, 0, 17, 5, 10, 0, 27, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0 ,  0,  0,  0, 29,  0,      0,  0,  0,  4,  1, 27,     25,  0,  0,  0,  0,  0,      0, 0, 36, 15, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 0, 13, 10, 0],
    [23, 25,  0, 35, 34,  0,      0,  0, 21, 29, 31,  0,      0,  0,  6,  0,  0,  0,      0, 16, 22, 0, 0, 0, 13, 0, 0, 0, 33, 0, 12, 0, 0, 14, 4, 27],
    [0 ,  0,  0,  0,  1,  0,      0,  0,  8,  0,  0, 10,      0,  0,  3,  0, 0, 29, 0, 9, 0, 0, 2, 0, 15, 0, 0, 0, 0, 0, 0, 19, 0, 24, 0, 0],
    [0 ,  0,  0,  0, 36,  0,      0, 35,  0, 25, 23,  0,      7,  0,  0, 22, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 28, 32, 0, 0, 3, 0, 29],
    [0 ,  0,  0,  0, 22, 19,      0,  0,  0,  0, 20,  0,      0,  0,  0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 31, 0, 0, 0, 0, 0, 0, 0, 0, 34],
    [0 ,  0,  0,  0,  0,  0,      0, 24, 18,  0,  7,  0,     12, 14,  0,  1, 0, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0],
    [0 , 23, 35,  0, 25,  9,     30,  0,  0,  0,  0,  0,      0,  0, 15, 36, 16, 0, 24, 0, 7, 26, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4],
    [0 , 21,  0,  0,  0,  0,      0,  0,  0,  1,  0,  0,      0,  9,  0, 25, 0, 0, 0, 0, 20, 11, 16, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0],
    [26,  0,  0,  0,  0,  0,      0,  0, 11,  6, 15,  0,      0,  0,  7, 13, 0, 18, 0, 33, 0, 8, 0, 0, 12, 0, 0, 1, 0, 29, 0, 0, 0, 0, 0, 0],
    [0 ,  0,  0, 32,  0,  0,      0, 12, 29,  0,  0,  0,      0,  0,  0,  6, 0, 0, 20, 36, 0, 0, 26, 19, 0, 0, 0, 0, 0, 0, 4, 0, 0, 5, 0, 8],
    [0 ,  0,  0,  0,  0, 25,     31, 21,  0,  0, 35,  0,      0,  0,  0, 19, 26, 0, 0, 22, 17, 0, 0, 13, 0, 0, 0, 33, 0, 0, 29, 1, 0, 0, 0, 28],
    [0 ,  0,  0,  0, 30,  0,      0,  0,  4,  0, 27,  0,      0,  0, 21,  0, 0, 0, 0, 0, 15, 0, 0, 6, 0, 16, 19, 0, 0, 26, 0, 0, 0, 0, 0, 0],
    [0 ,  0,  0,  0,  0,  0,      0,  0,  0,  0,  0,  0,      0,  0,  0,  0, 0, 28, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 26, 0, 0, 20, 19, 0],
    [0 ,  0,  0,  0,  0,  0,      0,  0,  0, 26,  0,  0,      0,  0, 33,  0, 0, 4, 0, 12, 1, 0, 28, 0, 0, 0, 0, 9, 0, 0, 0, 0, 25, 15, 0, 0],
    [25,  0,  0,  9,  0,  0,     12,  0,  0,  0, 29,  0,      0,  0,  0, 15, 36, 0, 0, 0, 0, 0, 22, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 14],
    [0 ,  0,  2,  0, 15, 23,      0,  0,  0,  0,  0,  0,      0, 20,  0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 28, 0, 30],
    [0 ,  0,  0, 13,  0,  0,      0,  0,  0,  0,  0,  0,      0,  0,  0, 27, 0, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0, 0, 0],
    [0 ,  0, 16,  0, 24,  0,      0,  0,  0,  0,  0,  0,     10,  0,  0,  0, 33, 0, 0, 0, 0, 14, 1, 0, 0, 0, 0, 0, 29, 31, 0, 21, 0, 0, 0, 0],
    [0 ,  0,  8,  0,  0,  0,      0,  0,  0,  0,  0,  0,      0,  0,  0,  0, 0, 30, 0, 0, 34, 9, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0, 0, 24, 19],
    [0 ,  0,  0,  0,  3,  0,      0,  8,  0,  0,  0, 14,      0,  0,  0, 35, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 22, 0, 0, 0, 0, 0, 0],
    [15,  0, 25,  0,  0,  0,      0,  0, 35,  0,  0,  0,      0,  0,  0,  0, 24, 0, 0, 0, 0, 0, 17, 18, 33, 5, 0, 10, 0, 0, 0, 4, 30, 1, 0, 0],
    [35,  0,  0,  0,  0,  0,      4,  0,  0, 28, 30,  0,      0,  0,  0,  0, 15, 0, 0, 0, 0, 20, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 33, 0, 0],
    [0 , 13,  0,  7,  0,  0,      0,  0,  0,  0,  0,  0,      0,  0, 33,  0, 0, 5, 1, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 35, 0, 0, 0, 25, 0, 0],
    [27, 14,  0,  0,  8, 10,      0,  0,  0,  0,  0,  0,      0,  0,  0, 28, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 36, 0, 0],
    [24,  0,  0,  0,  0,  0,      0,  0,  0,  2,  0,  0,      0,  0,  0,  0, 0, 0, 33, 10, 0, 5, 0, 0, 0, 0, 0, 0, 30, 3, 0, 0, 0, 31, 0, 0],
    [0 ,  0,  0,  0, 28,  4,      0,  0,  0,  0,  0,  5,      0,  0,  0,  0, 0, 0, 25, 0, 0, 0, 0, 2, 0, 20, 0, 11, 0, 0, 0, 26, 0, 0, 0, 0]
    ]
