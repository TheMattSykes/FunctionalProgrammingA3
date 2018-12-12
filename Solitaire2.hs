module Solitaire2 where
  
  import Solitaire1
  import MeanStats
  import Data.List
  
  -------------------------------------------------------------------------------------------
  
  findMoves :: EOBoard -> [EOBoard]
  findMoves (fnds,cols,res) = (reservesToColumns board [] res) ++ (columnsToReserves board [] cols) 
    ++ (columnSwitcher board [] cols)
    where board = (fnds,cols,res)
  
  reservesToColumns :: EOBoard -> [EOBoard] -> Reserves -> [EOBoard]
  reservesToColumns _ m [] = m
  reservesToColumns (fnds,cols,res) moves (x:xs) = 
    if x `elem` pcols then
      reservesToColumns (fnds,cols,res) (moves ++ [newM]) xs
    else
      reservesToColumns (fnds,cols,res) moves xs
    where hcols = map head cols -- heads of columns
          pcols = map (\f -> if(isAce f) then f else pCard f) hcols
          newM = (fnds, addToColumn cols x (length cols), delete x res)
  
  columnsToReserves :: EOBoard -> [EOBoard] -> Columns -> [EOBoard]
  columnsToReserves _ m [] = m
  columnsToReserves (fnds,cols,res) moves (x:xs)
    | length res < 8 = columnsToReserves (fnds,cols,res) (moves ++ [newMove]) xs
    | otherwise = columnsToReserves (fnds,cols,res) moves xs
    where (y:ys) = x
          newMove = (fnds, removeFromColumn cols y, res ++ [y])
          
          
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
    | board /= fBoard = Just fBoard
    | otherwise = 
        if (not $ null moves) then 
          Just toFoundations (findBestMove moves board ([],[],[]) 0)
        else Nothing
    where fBoard = toFoundations board
          moves = findMoves board
  
  
  findBestMove :: [EOBoard] -> EOBoard -> EOBoard -> Int -> EOBoard
  findBestMove [] _ bm _ = bm
  findBestMove ((fnds,cols,res):otherBoards) oriBoard bestBoard rank
    -- Check to see if Kings in previously vacent column
    | (length (kings)) > 0 && kings /= (filter isKing (map head oriCols)) 
      && (length cols) > (length oriCols) =
          findBestMove otherBoards oriBoard (fnds,cols,res) 1
    | (length res) > (length oriRes) =
        if rank >= 2 || rank == 0 then findBestMove otherBoards oriBoard (fnds,cols,res) 2
        else findBestMove otherBoards oriBoard bestBoard rank
    | otherwise = 
        if rank >= 3 || rank == 0 then findBestMove otherBoards oriBoard (fnds,cols,res) 3
        else findBestMove otherBoards oriBoard bestBoard rank
    where kings = filter isKing (map head cols)
          (oriFnds,oriCols,oriRes) = oriBoard
  
  -------------------------------------------------------------------------------------------
  
    {-
    
    Old Ideas 
    
    -}
    
    {-
    reservesToColumns :: EOBoard -> EOBoard
    reservesToColumns (fnds,cols,res) = 
    (fnds,
    cols,
    map head (map (\f -> if (not(isAce f))&&(elem (sCard f) hcols) then [] else [f]) [(Two,Diamonds),(Four,Diamonds)]))
    where hcols = map head cols
    --(filter (\r-> (not(elem (pCard r) fnds )))res)) 
    -}
    
    {-
    pcolsList = (map (\f -> if(isAce f) then [] else [pCard f]) hcols)
    pcols = map head (filter (not.null) pcolsList)
    -}
  