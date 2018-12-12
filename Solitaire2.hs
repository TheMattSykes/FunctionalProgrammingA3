module Solitaire2 where
  
  import Solitaire1
  import MeanStats
  import Data.List
  
  -------------------------------------------------------------------------------------------
  
  findMoves :: EOBoard -> [EOBoard]
  findMoves board = [fboard]
    where fboard = toFoundations board
  
  reservesToColumns :: EOBoard -> [EOBoard] -> Reserves -> [EOBoard]
  reservesToColumns _ m [] = m
  reservesToColumns (fnds,cols,res) moves (x:xs) = 
    if x `elem` pcols then
      reservesToColumns (fnds,cols,res) (moves ++ [newM]) xs
    else
      reservesToColumns (fnds,cols,res) moves xs
    where hcols = map head cols -- heads of columns
          pcols = map (\f -> if(isAce f) then f else pCard f) hcols
          newM = (fnds, addToColumn cols x, delete x res)
  
  columnsToReserves :: EOBoard -> [EOBoard] -> Columns -> [EOBoard]
  columnsToReserves _ m [] = m
  columnsToReserves (fnds,cols,res) moves (x:xs)
    | length res < 8 = columnsToReserves (fnds,cols,res) (moves ++ [newMove]) xs
    | otherwise = columnsToReserves (fnds,cols,res) moves xs
    where (y:ys) = x
          newMove = (fnds, cols, res ++ [y])
  
  {-
    addToColumn
    Takes the columns and a new card.
    If the card is a predecessor to one of the columns it is added to the head.
  -}
  addToColumn :: Columns -> Card -> Columns
  addToColumn [] _ = [] -- base case
  addToColumn ((y:ys):xs) c
    -- If head card is an ace then no predecessor
    | not (isAce y) =
        -- Predecssor of y is c so add to head
        if pCard y == c then
          newHead:xs -- function finished
        else
          def
    | otherwise = def
    where def = (y:ys):addToColumn xs c -- default recursive call
          newHead = c:(y:ys) -- new head with card added
          
          
  removeFromColumn :: Columns -> Card -> Columns
  removeFromColumn [] _ = []
  removeFromColumn ((y:ys):xs) c
    | y == c = ys:xs
    | otherwise = removeFromColumn xs c
          
  
  
  chooseMove :: EOBoard -> Maybe EOBoard
  chooseMove board = Just (head (findMoves board))
  
  
  
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
  