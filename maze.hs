import qualified Data.Set as Set
import System.Random
import System.IO.Unsafe

data Maze = Maze { cells :: [(Bool, Bool)]  -- [(rightWall, downWall)]
                 , width :: Int
                 , height :: Int
                 }

rand :: Int -> Int
-- Returns a random integer from 0 to max-1
rand max = unsafePerformIO $ randomRIO (0, max-1)

shuffle :: [a] -> [a]
-- Randomly shuffles a list
shuffle = unsafePerformIO . shuffleM

shuffleM :: [a] -> IO [a]
-- DON'T BOTHER! Helper for shuffle
shuffleM [] = return []
shuffleM n = do {
                r <- fmap (flip mod $ length n) randomIO;
                n1 <- return $ n !! r;
                fmap ((:) n1) $ shuffleM $ (take r n) ++ (drop (r+1) n)
             }
-----------------------------------------Question 1-----------------------------------------

---Creates a list of (True,True) tuples---
makeMaze1::Int->[(Bool, Bool)]
makeMaze1 1 = [(True, True)]
makeMaze1 x = (True, True):makeMaze1 (x - 1)

---Creates a maze using the tuples above---
makeMaze::Int->Int->Maze
makeMaze x y = Maze (makeMaze1 (x*y)) x y

-----------------------------------------Question 2-----------------------------------------

---Creates a set for every cell---
makeSet x n = if x == n then [Set.fromList [n]] else Set.fromList [x]:makeSet (x + 1) n

---Creates all the possible wall to be created---
makeWall::Int->Int->Int->[(Int,Int)]
makeWall n width height | n == (width*height -1) = []
                        | ((div n width) == (div (n+1) width)) && ((div n width) < (height-1)) = [(n,n+1),(n,n+width)] ++ makeWall (n+1) width height
                        | ((div n width) == (div (n+1) width)) = [(n,n+1)] ++ makeWall (n+1) width height
                        | div n width < (height-1) = [(n,n+width)] ++ makeWall (n+1) width height

---Removes the wall between the x and y cell---
removeWall n x y (h:cell) width | n == x = if (((x + 1) == y) && (width > 1)) then (False, snd h):cell else (fst h, False):cell
                                | otherwise = h:removeWall (n + 1) x y cell width

---Unifies the sets of two cells when the wall between them is removed---
updateSet set joinedSet [] n = set
updateSet (h1:set) joinedSet (h2:elements) n | n == h2 = joinedSet:updateSet set joinedSet elements (n + 1)
                                             | otherwise = h1:updateSet set joinedSet (h2:elements) (n + 1)

---Returns a list of tuples, where each tuple is a cell (Bool,Bool) depending on the existance of a wall---
kruskal1 cell set [] width = cell
kruskal1 cell set (x:wall) width = if (not(Set.member (fst x) (set !! snd x)) && not(Set.member (snd x) (set !! fst x))) then let joinedSet = Set.union (set !! fst x) (set !! snd x) in kruskal1 (removeWall 0 (fst x) (snd x) cell width) (updateSet set joinedSet (Set.elems joinedSet) 0) wall width else kruskal1 cell set wall width

---Returns a perfect maze---
kruskal::Maze -> Maze
kruskal maze = Maze (kruskal1 (cells maze) (makeSet 0 ((width maze)*(height maze) - 1)) (shuffle(makeWall 0 (width maze) (height maze))) (width maze)) (width maze) (height maze) --na kanei shuffle walls

-----------------------------------------Question 3-----------------------------------------

---A DFS solving function for the maze, returns the path to the target with some additional information for the developers, the move 1,2,3,4 options represent the move to north, east, south, west---
solvePerfect1 maze currentCell target lastMove move | (currentCell == target) = [(-1,-1),(mod currentCell (width maze), div currentCell (width maze))]
                                                    | (move == 1) && (currentCell - (width maze) >= 0) && (currentCell - (width maze) /= lastMove) && (not (snd((cells maze) !! (currentCell - (width maze))))) = let flag = solvePerfect1 maze (currentCell - (width maze)) target currentCell 1 in if fst(flag!!0) == -1 then [(-1,-1)]++[(mod currentCell (width maze), div currentCell (width maze))]++tail(flag) else solvePerfect1 maze currentCell target lastMove 2
                                                    | (move == 2) && (div (currentCell + 1) (width maze) == div currentCell (width maze)) && (currentCell + 1 /= lastMove) && (not (fst((cells maze) !! (currentCell)))) = let flag = solvePerfect1 maze (currentCell + 1) target currentCell 1 in if fst(flag!!0) == -1 then [(-1,-1)]++[(mod currentCell (width maze), div currentCell (width maze))]++tail(flag) else solvePerfect1 maze currentCell target lastMove 3
                                                    | (move == 3) && ((currentCell + (width maze)) <= ((width maze)*(height maze) - 1)) && (currentCell + (width maze) /= lastMove) && (not (snd((cells maze) !! (currentCell)))) = let flag = solvePerfect1 maze (currentCell + (width maze)) target currentCell 1 in if fst(flag!!0) == -1 then [(-1,-1)]++[(mod currentCell (width maze), div currentCell (width maze))]++tail(flag) else solvePerfect1 maze currentCell target lastMove 4
                                                    | (move == 4) && (div (currentCell - 1) (width maze) == div currentCell (width maze)) && (currentCell - 1 /= lastMove) && (not (fst((cells maze) !! (currentCell - 1)))) = let flag = solvePerfect1 maze (currentCell - 1) target currentCell 1 in if fst(flag!!0) == -1 then [(-1,-1)]++[(mod currentCell (width maze), div currentCell (width maze))]++tail(flag) else solvePerfect1 maze currentCell target lastMove 5
                                                    | (move == 5) = [(-2,-2)]
                                                    | otherwise = solvePerfect1 maze currentCell target lastMove (move + 1)

---Returns just the path from the start to the target cell---
solvePerfect :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
solvePerfect maze start target = tail(solvePerfect1 maze ((width maze)*(snd start) + (fst start)) ((width maze)*(snd target) + (fst target)) (-1) 1)

-----------------------------------------Question 4-----------------------------------------

---Converts each cell (represented with coordinates) to a unique cell id---
pathToInt [] width = []
pathToInt (x:xs) width = ((snd x)*width + (fst x)):pathToInt xs width

---Creates the string to be printed for the odd printing lines---
oddPrint :: Maze -> [Int] -> Int -> Int -> [Char]
oddPrint maze path currWidth currHeight | (currWidth == (width maze)) = "\n"
                                        | (path /= []) && ((currHeight*(width maze) + currWidth) == head(path)) = if (fst((cells maze) !! (currHeight*(width maze) + currWidth))) then " * |" ++ oddPrint maze (tail(path)) (currWidth + 1) currHeight else " *  " ++ oddPrint maze (tail(path)) (currWidth + 1) currHeight
                                        | otherwise = if (fst((cells maze) !! (currHeight*(width maze) + currWidth))) then "   |"  ++ oddPrint maze path (currWidth + 1) currHeight else "    " ++ oddPrint maze path (currWidth + 1) currHeight

---Creates the string to be printed for the even printing lines---
evenPrint :: Maze -> Int -> Int -> [Char]
evenPrint maze currWidth currHeight  | currWidth == (width maze) = "\n"
                                     | otherwise = if (snd((cells maze) !! (currHeight*(width maze) + currWidth))) then "---+" ++ evenPrint maze (currWidth + 1) currHeight else "   +" ++ evenPrint maze (currWidth + 1) currHeight

---Copyrights reserved to PRondo---
quickSort [] = []
quickSort (x:xs) = quickSort [y | y<-xs , y<x] ++ [x] ++ quickSort [y | y<-xs , y>=x]

---Drops the numbers of the list that are less than n---
dropLess [] n = []
dropLess (x:path) n = if x < n then dropLess path n else (x:path)

---Unify the odd and the even strings to be printed---
showMaze1 :: Maze -> [Int] -> Int -> [Char]
showMaze1 maze path n | n == (height maze) = "\n"
                      | otherwise = "|" ++(oddPrint maze path 0 n) ++ "+" ++ (evenPrint maze 0 n) ++ (showMaze1 maze (dropLess path ((n+1)*(width maze))) (n+1))

---Prints the unified string representing the maze---
showMaze maze path = "+" ++ (evenPrint (makeMaze (width maze) 1) 0 0) ++ showMaze1 maze (quickSort(pathToInt path (width maze))) 0

-----------------------------------------Question Bonus 1-----------------------------------------

---Counts with how many cells there is connection, k represent the direction to check with k 1,2,3,4 representing east, south, north, west---
countNeighboors k n maze | k == 5 = 0
                         | k == 1 = if((fst((cells maze) !! n)) == False) then 1+ countNeighboors (k+1) n maze else 0+countNeighboors (k+1) n maze
                         | k == 2 = if((snd((cells maze) !! n)) == False) then 1+ countNeighboors (k+1) n maze else 0+countNeighboors (k+1) n maze
                         | k == 3 && ((n - (width maze)) > 0) = if((snd((cells maze) !! (n - (width maze)))) == False) then 1+ countNeighboors (k+1) n maze else 0+countNeighboors (k+1) n maze
                         | k == 4 && (div n (width maze) == div (n-1) (width maze)) = if((fst((cells maze) !! (n-1))) == False) then 1+ countNeighboors (k+1) n maze else 0+countNeighboors (k+1) n maze
                         | otherwise = 0 + countNeighboors (k+1) n maze

---Removes an existing wall of the cell n (n is the cell id refered above), flag 1 reprsents removal of the east wall, flag 2 reprsents removal of the south wall---
modify flag k n cells | k == n && flag == 1 = (False,snd(head cells)):tail(cells)
                      | k == n && flag == 2 = (fst(head cells),False):tail(cells)
                      | otherwise = (head cells):modify flag (k+1) n (tail cells)

---Deletes a wall so the cell is no more a dead-end, k represent the direction to check with k 1,2,3,4 representing east, south, north, west---
deletewall k n maze | k == 1 && (mod (n+1) (width maze) /= 0) = if((fst((cells maze) !! n)) == True) then Maze (modify 1 0 n (cells maze)) (width maze) (height maze) else deletewall (k+1) n maze
                    | k == 2 && (n+(width maze)) < ((width maze) * (height maze))  = if((snd((cells maze) !! n)) == True) then Maze (modify 2 0 n (cells maze)) (width maze) (height maze) else deletewall (k+1) n maze
                    | k == 3 && ((n - (width maze)) > 0)  = if((snd((cells maze) !! (n - (width maze)))) == True) then Maze (modify 2 0 (n-(width maze)) (cells maze)) (width maze) (height maze) else deletewall (k+1) n maze
                    | k == 4 && (div n (width maze) == div (n-1) (width maze)) = if((fst((cells maze) !! (n-1))) == True) then Maze (modify 1 0 (n-1) (cells maze)) (width maze) (height maze) else deletewall (k+1) n maze
                    | k == 5 = maze
                    | otherwise = deletewall (k+1) n maze

---Returns a braided maze---
braid1 :: Int -> Maze -> Maze
braid1 n maze | n == ((width maze) * (height maze)) = maze
              | otherwise = let x = countNeighboors 1 n maze in if x < 2 then braid1 (n+1) (deletewall 1 n maze) else braid1 (n+1) maze

---Returns a braided maze---
braid :: Maze -> Maze
braid maze = braid1 0 maze

-----------------------------------------Question Bonus 2-----------------------------------------

---Returns True upon finding the n cell in the frontier list of tuples (cell,path), else false---
inFrontier [] n = False
inFrontier (h:frontier) n | fst(h) == n = True
                          | otherwise = inFrontier frontier n

---A BFS solving function for the braided maze, returns the path to the target, the move 1,2,3,4 options represent the move to north, east, south, west---
solveBraid1 maze goal explored frontier move | current == goal = path
                                             | (move == 1) && (current - (width maze) >= 0) && (not(Set.member (current - (width maze)) explored)) && (not(inFrontier frontier (current - (width maze)))) && (not(snd((cells maze) !! (current - (width maze))))) = solveBraid1 maze goal explored (frontier++[(current - (width maze),path++[(mod (current - (width maze)) (width maze), div (current - (width maze)) (width maze))])]) (move+1)
                                             | (move == 2) && (div (current + 1) (width maze) == div current (width maze)) && (not(Set.member (current + 1) explored)) && (not(inFrontier frontier (current +1 ))) && (not (fst((cells maze) !! (current)))) = solveBraid1 maze goal explored (frontier++[(current + 1,path++[(mod (current +1) (width maze), div (current +1) (width maze))])]) (move+1)
                                             | (move == 3) && ((current + (width maze)) <= ((width maze)*(height maze) - 1)) && (not(Set.member (current + (width maze)) explored)) && (not(inFrontier frontier (current + (width maze)))) && (not (snd((cells maze) !! (current)))) = solveBraid1 maze goal explored (frontier++[(current + (width maze),path++[(mod (current +(width maze)) (width maze), div (current +(width maze)) (width maze))])]) (move+1)
                                             | (move == 4) && (div (current - 1) (width maze) == div current (width maze)) && (not(Set.member (current - 1) explored)) && (not(inFrontier frontier (current - 1))) && (not (fst((cells maze) !! (current - 1)))) = solveBraid1 maze goal explored (frontier++[(current -1,path++[(mod (current -1) (width maze), div (current -1) (width maze))])]) (move+1)
                                             | (move == 5) = solveBraid1 maze goal (Set.union explored (Set.fromList [current])) (tail frontier) 1
                                             | otherwise = solveBraid1 maze goal explored frontier (move+1)
    where (current,path) = head(frontier)

---Returns the path from the start cell to the target cell---
solveBraid :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
solveBraid maze start target = let x = ((width maze)*(snd start) + (fst start)) in solveBraid1 maze ((width maze)*(snd target) + (fst target)) (Set.fromList []) [(x,[start])] 0
--------------------------------------------------------------------------------------------------
