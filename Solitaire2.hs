module Solitaire2 where
  
  import Solitaire1
  import MeanStats
  import Data.List
  
  -------------------------------------------------------------------------------------------
  
  -- findMoves
  -- Find all posible moves from current position.
  findMoves :: EOBoard -> [EOBoard]
  -- Compile list of possible moves
  findMoves (fnds,cols,res) = (reservesToColumns board [] res) ++ (columnsToReserves board [] cols) ++ (columnSwitcher board [] cols)
    -- Split board into tuple elemets
    where board = (fnds,cols,res)
  
  
  -- reservesToColumns
  -- Check each reserve to see if that card can be added to the front of a column
  reservesToColumns :: EOBoard -> [EOBoard] -> Reserves -> [EOBoard]
  reservesToColumns _ m [] = m
  reservesToColumns (fnds,cols,res) moves (x:xs) = 
    -- if the reserve card is a predecessor of one of the columns
    if x `elem` pcols then
      -- add move
      reservesToColumns (fnds,cols,res) (moves ++ [newM]) xs
    else
      -- don't include move
      reservesToColumns (fnds,cols,res) moves xs
    where hcols = if (length cols) > 0 then (map head cols) else [] -- heads of columns where length > 0
          pcols = map (\f -> if(isAce f) then f else pCard f) hcols -- Predecessor cards of top of columns
          -- Provisionally add card to columns and remove from reserves, store as new move
          newM = (fnds, addToColumn cols x (length cols), delete x res)
          
  
  columnsToReserves :: EOBoard -> [EOBoard] -> Columns -> [EOBoard]
  columnsToReserves _ m [] = m
  columnsToReserves (fnds,cols,res) moves (x:xs)
    | length res < 8 && not (isKing c1) && length x > 1 = 
        if sCard c1 == head c2 then columnsToReserves (fnds,cols,res) moves xs
        else columnsToReserves (fnds,cols,res) (moves ++ [newMove]) xs
    | otherwise = columnsToReserves (fnds,cols,res) moves xs
    where (c1:c2) = x
          newMove = (fnds, removeFromColumn cols c1, res ++ [c1])
          
          
  columnSwitcher :: EOBoard -> [EOBoard] -> Columns -> [EOBoard]
  columnSwitcher _ m [] = m
  columnSwitcher (fnds,cols,res) moves (x:xs)
    | cols /= colChangeCheck = columnSwitcher (fnds,cols,res) (moves ++ [newMove]) xs
    | otherwise = columnSwitcher (fnds,cols,res) moves xs
    where (c:ys) = x
          colChangeCheck = addToColumn cols c (length cols)
          newCols = addToColumn (removeFromColumn cols c) c (length cols)
          newMove = (fnds,newCols,res)
  
  
  {-
    addToColumn
    Takes the columns and a new card.
    If the card is a predecessor to one of the columns it is added to the head.
  -}
  addToColumn :: Columns -> Card -> Int -> Columns
  -- Add to new column if less than 6 columns filled
  addToColumn [] c len = if len < 6 then [[c]] else []
  addToColumn ((y:ys):xs) c l
    -- If head card is an ace then no predecessor
    | not (isAce y) =
        -- Predecssor of y is c so add to head
        if pCard y == c then
          newHead:xs -- function finished
        else
          def
    | otherwise = def
    where def = (y:ys):addToColumn xs c l -- default recursive call
          newHead = c:(y:ys) -- new head with card added

  
  removeFromColumn :: Columns -> Card -> Columns
  removeFromColumn [] _ = []
  removeFromColumn ((y:ys):xs) c
    | y == c = ys:xs
    | otherwise = (y:ys):removeFromColumn xs c
          
  
  ------------------------------------------------------------------------------------------- 
  chooseMove :: EOBoard -> Maybe EOBoard
  chooseMove board
    | moves == [] = Nothing
    | board /= fBoard = Just fBoard
    | otherwise = 
        if (not $ null moves) then 
          Just $ toFoundations (findBestMove moves board ([],[],[]) 0)
        else Nothing
    where fBoard = toFoundations board
          moves = findMoves board
  
  
  findBestMove :: [EOBoard] -> EOBoard -> EOBoard -> Int -> EOBoard
  findBestMove [] _ bm _ = bm
  findBestMove ((fnds,cols,res):otherBoards) oriBoard bestBoard rank
    -- Check to see if Kings in previously vacent column
    | rank <= 3 && (length (kings)) > 0 && kings /= (filter isKing (map head oriCols)) 
      && (length cols) > (length oriCols) =
          findBestMove otherBoards oriBoard (fnds,cols,res) 3
    | rank <= 1 && (length res) > (length oriRes) =
        findBestMove otherBoards oriBoard (fnds,cols,res) 1
    | otherwise = 
        if rank >= 3 || rank == 0 then findBestMove otherBoards oriBoard (fnds,cols,res) 2
        else findBestMove otherBoards oriBoard bestBoard rank
    where kings = if (length cols > 0) then filter isKing (map head cols) else []
          (oriFnds,oriCols,oriRes) = oriBoard
  
  
  -------------------------------------------------------------------------------------------
  eOGame :: EOBoard -> IO String
  eOGame board = do
    let (board, score) = runEOGame board 0
    let win = score == 52
    putStr "EOGame"
    putStrLn "Board: "
    putStr (show board)
    putStr "\n\nScore: "
    putStr (show score)
    putStr "\nWin: "
    putStrLn (show win)
    return ""
  
  runEOGame :: EOBoard -> Int -> (EOBoard, Int)
  runEOGame b s = 
    case move of 
      Nothing -> (b, s)
      Just move -> if move /= b then runEOGame move (s+1) else (b, s)
    where move = chooseMove b
  
  
  -------------------------------------------------------------------------------------------
  eOExpt :: Int -> [(EOBoard,Int)]
  eOExpt seed = runExp seed 1 []
  
  runExp :: Int -> Int -> [EOBoard] -> [(EOBoard,Int)]
  runExp seed count runs = 
    if count < 100 then (result, wins):(runExp seed (count+1) runs)
    else [(result, wins)]
    where (result, wins) = (runEOGame (eODeal 30) 0)
    
  