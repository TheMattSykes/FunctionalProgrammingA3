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
          
  -- columnsToReserves
  -- check each head of column to see if it can be added to the reserves when spaces are free
  columnsToReserves :: EOBoard -> [EOBoard] -> Columns -> [EOBoard]
  columnsToReserves _ m [] = m
  columnsToReserves (fnds,cols,res) moves (x:xs)
    -- reserves have maximum size of 8
    -- check if card can be added to reserve
    | length res < 8 && not (isKing c1) && length x > 1 = 
        -- If card above head is successor do not move to reserve
        if sCard c1 == head c2 then columnsToReserves (fnds,cols,res) moves xs
        -- add move otherwise
        else columnsToReserves (fnds,cols,res) (moves ++ [newMove]) xs
    | otherwise = columnsToReserves (fnds,cols,res) moves xs
    where (c1:c2) = x -- h:t pattern causes error for c2, not included
          newMove = (fnds, removeFromColumn cols c1, res ++ [c1])
          
  -- columnSwitcher
  -- moves cards between columns
  columnSwitcher :: EOBoard -> [EOBoard] -> Columns -> [EOBoard]
  columnSwitcher _ m [] = m
  columnSwitcher (fnds,cols,res) moves (x:xs)
    -- if altered columns not equal to existing columns
    | cols /= colChangeCheck = columnSwitcher (fnds,cols,res) (moves ++ [newMove]) xs
    -- do not add move
    | otherwise = columnSwitcher (fnds,cols,res) moves xs
    where (c:ys) = x
          colChangeCheck = addToColumn cols c (length cols) -- check if columns have changed
          newCols = addToColumn (removeFromColumn cols c) c (length cols) -- switch card between columns
          newMove = (fnds,newCols,res)
  
  
  
  -- addToColumn
  -- Takes the columns and a new card.
  -- If the card is a predecessor to one of the columns it is added to the head.
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

  -- removeFromColumn
  -- Taskes the columns and a card to remove
  -- Checks the heads of the columns to see if the card matches one and removes if a match is found
  removeFromColumn :: Columns -> Card -> Columns
  removeFromColumn [] _ = []
  removeFromColumn ((y:ys):xs) c
    | y == c = ys:xs -- remove card from column
    | otherwise = (y:ys):removeFromColumn xs c
          
  
  -------------------------------------------------------------------------------------------
  
  -- chooseMove
  -- Takes an EOBoard and either returns an EOBoard or nothing if no move is found
  chooseMove :: EOBoard -> Maybe EOBoard
  chooseMove board
    -- if moves empty return nothing
    | moves == [] = Nothing
    -- cards can be moved to the foundations so return as best move
    | board /= fBoard = Just fBoard
    | otherwise = 
        -- if moves not null then find the best moves
        if (not $ null moves) then 
          Just $ toFoundations (findBestMove moves board ([],[],[]) 0)
        else Nothing
    where fBoard = toFoundations board
          -- create list of available moves
          moves = findMoves board
  
  -- findBestMove
  -- Rank different moves based on the likeliness they will allow the player to win
  findBestMove :: [EOBoard] -> EOBoard -> EOBoard -> Int -> EOBoard
  findBestMove [] _ bm _ = bm
  findBestMove ((fnds,cols,res):otherBoards) oriBoard bestBoard rank
    -- Check to see if Kings in previously vacent column
    | rank <= 3 && (length (kings)) > 0 && kings /= (filter isKing (map head oriCols)) 
      && (length cols) > (length oriCols) =
      -- Add move as best
      findBestMove otherBoards oriBoard (fnds,cols,res) 3
    -- Check to see if move will increase the amount of reserve cards
    | rank <= 1 && (length res) > (length oriRes) =
      -- Add move as best
      findBestMove otherBoards oriBoard (fnds,cols,res) 1
    | otherwise = 
        -- Add move as best
        if rank >= 3 || rank == 0 then findBestMove otherBoards oriBoard (fnds,cols,res) 2
        -- Do not add move as best
        else findBestMove otherBoards oriBoard bestBoard rank
    where kings = if (length cols > 0) then filter isKing (map head cols) else []
          (oriFnds,oriCols,oriRes) = oriBoard
  
  
  -------------------------------------------------------------------------------------------
  -- eOGame 
  -- show the results of running a game and choosing moves
  eOGame :: EOBoard -> IO String
  eOGame board = do
    let (b, s) = runEOGame board 0
    let win = s == 52
    putStr "EOGame"
    putStrLn "Board: "
    putStr (show b)
    putStr "\n\nScore: "
    putStr (show s)
    putStr "\nWin: "
    putStrLn (show win)
    return ""
  
  -- runEOGame
  -- call chooseMove until no more moves are available, increase score each time a move is made
  runEOGame :: EOBoard -> Int -> (EOBoard, Int)
  runEOGame b s = 
    case move of 
      Nothing -> (b, s)
      Just move -> if move /= b then runEOGame move (s+1) else (b, s)
    where move = chooseMove b
  
  
  -------------------------------------------------------------------------------------------
  -- eOExpt
  -- Returns the list of boards and scores for 100 games
  eOExpt :: Int -> [(EOBoard,Int)]
  eOExpt seed = runExp seed 1 []
  
  -- runExp
  -- run 100 games based on a random seed and return the boards and results of those games
  runExp :: Int -> Int -> [EOBoard] -> [(EOBoard,Int)]
  runExp seed count runs = 
    if count < 100 then (result, wins):(runExp seed (count+1) runs)
    -- 100 games completed
    else [(result, wins)]
    where (result, wins) = (runEOGame (eODeal seed) 0)
    
  