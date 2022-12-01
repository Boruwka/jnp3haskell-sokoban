{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
type Program = IO ()

main :: Program
main = walk

-- Jagoda Bracha jb429153
-- nie zdążę całości na pewno, do jutra do 18:00 wyślę nową wersję, jeśli uda mi się coś dorobić
-- na razie brakuje całego etapu 5, funkcji isSane i zrobienia przeszukiwania grafu tak, by się liczyło w sensownym czasie 
-- (i to też jest konieczne i wystarczające do zrobienia funkcji isSane) 


walk :: IO()
walk = runActivity main_activity

-- resetowanie gry i start screen

runActivity :: Activity s -> IO ()
runActivity a = activityOf (actState a) (actHandle a) (actDraw a)

main_activity :: Activity (SSState State) 
main_activity = withStartScreen (resettable (
    Activity {
        actState = initial_state,
        actHandle = handleEvent,
        actDraw = draw }
    ))

data Activity world = Activity {
    actState  :: world,
    actHandle :: (Event -> world -> world),
    actDraw   ::(world -> Picture)
    }

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s
        
data SSState world = StartScreen | Running world

withStartScreen :: Activity s -> Activity (SSState s)

withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s
    
startScreen :: Picture
startScreen = etap4 

etap4 :: Picture
etap4 = pictureOfBools (mapList (\x -> isClosed x) (mazes ++ badMazes))

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k / 2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)


-- pierwotny stan 

initial_state :: State
initial_state = S (C 0 1) 10 (initial_maze) (initial_maze) L initialBoxes

initial_maze :: Coord -> Tile
initial_maze (C x y)
  | abs x > 4  || abs y > 4  = Blank  -- blank
  | abs x == 4 || abs y == 4 = Wall  -- wall
  | x ==  2 && y <= 0        = Wall  -- wall
  | x ==  3 && y <= 0        = Storage  -- storage
  | x >= -2 && y == 0        = Box  -- box
  | otherwise                = Ground  -- ground

n :: Int 
n = 10

initialBoxes :: [Coord]
initialBoxes = getBoxes initial_maze


-- funkcje obsługujące zdarzenia

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state
    | (isWinning state) = state
    | key == "Right" = move_player R state
    | key == "Up"    = move_player U state
    | key == "Left"  = move_player L state
    | key == "Down"  = move_player D state
handleEvent _ c      = c
  

move_player :: Direction -> State -> State
move_player dir state =
    if (check_coord state (moveCoord dir (stPlayer state)))
    then 
        state {
            stPlayer = 
            (moveCoord dir (stPlayer state)),
            stDir = dir
        }
    else 
        if (check_if_box_is_moved state dir (moveCoord dir (stPlayer state))) -- to pudło i za nim jest miejsce
        then 
            state { 
                stPlayer = (moveCoord dir (stPlayer state)),
                stBoxes = new_boxes,                    
                stMaze = addBoxes new_boxes (removeBoxes initial_maze),
                stDir = dir
            }
        else 
            state
    where new_boxes = map (change_one_element (moveCoord dir (stPlayer state)) (moveCoord dir (moveCoord dir (stPlayer state)))) (stBoxes state)
        
         
        
change_one_element :: Coord -> Coord -> Coord -> Coord 
change_one_element a b c = 
    if a == c 
    then b
    else c

    

-- funkcja rysująca
    
draw :: State -> Picture
draw state = 
    if (isWinning state) 
    then lettering("Wygrana!")
    else 
        (translated (fromIntegral (x (stPlayer state))) (fromIntegral (y (stPlayer state))) (player (stDir state))) &
        (pictures[
        draw_square state (C x y) | 
        x <- range_n (stRange state), y <- range_n (stRange state) ])
    
-- rysunki elementów planszy 

wall :: Picture
wall = colored black (solidRectangle 1 1)

ground :: Picture
ground = colored (light blue) (solidRectangle 1 1)

storage :: Picture
storage = 
    colored (red) (solidCircle 0.5) & 
    colored (light blue) (solidRectangle 1 1) 
    
box :: Picture
box = 
    (colored black (polyline [(0.5, 0.5), (-0.5, -0.5)])) & 
    (colored black (polyline [(-0.5, 0.5), (0.5, -0.5)])) & 
    (colored black (rectangle 1 1)) & 
    (colored brown (solidRectangle 1 1))
    
-- manipulowanie skrzyniami

check_if_box_is_moved :: State -> Direction -> Coord -> Bool
check_if_box_is_moved state dir c = 
    (stMaze state c) == Box && ((stMaze state (moveCoord dir c)) == Ground || (stMaze state (moveCoord dir c)) == Storage)

getBoxes :: (Coord -> Tile) -> [Coord]
getBoxes maze = [(C x y) | x <- range_n n, y <- range_n n, maze (C x y) == Box]

addBoxes :: [Coord] -> (Coord -> Tile) -> (Coord -> Tile)
addBoxes boxes maze = maze_with_boxes
    where 
        maze_with_boxes c = if (elem c boxes)
        then 
            Box
        else 
            maze c

removeBoxes :: (Coord -> Tile) -> Coord -> Tile
removeBoxes maze =  fun . maze
    where
        fun Box = Ground
        fun tile = tile
        
        
         
pictureOfBoxes :: [Coord] -> Picture
pictureOfBoxes boxes = 
    (pictures[
    draw_box c | c <- boxes])
    
-- poziomy


data Maze = Maze {
    mazeStart  :: Coord,
    mazeMap :: Coord -> Tile
    }

mazes :: [Maze]
mazes = [
    Maze (C 0 1) initial_maze,
    Maze (C (-2) (-2)) easy_testMaze_GN,
    Maze (C 1 (-1)) easy_spiralMaze_DM
    ]
    
easy_testMaze_GN :: Coord -> Tile
easy_testMaze_GN (C x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground
  
easy_spiralMaze_DM :: Coord -> Tile
easy_spiralMaze_DM (C x y)
    | abs x >  4 || abs y > 4      = Blank
    | abs x == 4                   = Wall
    | abs y == 4                   = Wall
    | x ==  2 && y <=  1           = Wall
    | x >= -1 && x <=  2 && y == 1 = Wall
    | x == -1 && y >= -1 && y <= 1 = Wall
    | x ==  0 && y == -1           = Box
    | x ==  3 && y == -3           = Storage
    | otherwise                    = Ground



badMazes :: [Maze]
badMazes = [
  Maze (C (-2) (-2)) badTestMaze_BS,
  Maze (C 1 (-1))    cutOffStorageMaze_DM
  ]

badTestMaze_BS :: Coord -> Tile
badTestMaze_BS (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2                  = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

cutOffStorageMaze_DM :: Coord -> Tile
cutOffStorageMaze_DM (C x y)
  | abs x > 7 || abs y > 7                            = Blank
  | abs x == 7                                        = Wall
  | abs y == 7                                        = Wall
  | x >= 4 && y == 4                                  = Wall
  | x == 4 && y >= 4                                  = Wall
  | x >= 5 && y >= 5                                  = Storage
  | elem (x, y) [(-6, 6), (-6, -6), (6, -6), (6, -5)] = Storage
  | x == 0 && elem y [-4 .. 2]                        = Box
  | otherwise                                         = Ground

    
-- podstawowe definicje

isWinning :: State -> Bool 
isWinning state = andList (map (is_on_storage state) (stBoxes state))
        
is_on_storage :: State -> Coord -> Bool
is_on_storage state c = ((stInitialMaze state) c == Storage)
    

data Direction = R | U | L | D
data Coord = C {x :: Int, y :: Int} deriving (Eq)

moveCoord :: Direction -> Coord -> Coord
moveCoord R (C x y) = C (x+1) y
moveCoord U (C x y) = C x (y+1)
moveCoord L (C x y) = C (x-1) y
moveCoord D (C x y) = C x (y-1)

moveCoords :: [Direction] -> Coord -> Coord
moveCoords [] initial_coord = initial_coord
moveCoords (h:tab) initial_coord = 
    moveCoords tab (moveCoord h initial_coord)

check_coord :: State -> Coord -> Bool -- sprawdza czy można się przemieścić na to pole
check_coord state c = (stMaze state c) == Ground || (stMaze state c) == Storage

draw_square :: State -> Coord -> Picture 
draw_square state c = translated (fromIntegral (x c)) (fromIntegral (y c)) (drawTile ((stMaze state) c))

player_base :: Picture
player_base = colored yellow (
    (solidPolygon [(0.5, 0.5), (0.5, 0), (0, 0.25)]) &
    (polyline [(0.5, 0), (0.5, -0.5)]) &
    (polyline [(0.5, 0), (0, -0.5)]))
    
    
player :: Direction -> Picture 
player R = rotated 3.14 player_base
player U = rotated 4.71 player_base
player L = player_base
player D = rotated 1.57 player_base


data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

drawTile :: Tile -> Picture 
drawTile Wall = wall 
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

data State = S {
    stPlayer :: Coord,
    stRange  :: Int,
    stMaze   :: (Coord -> Tile),
    stInitialMaze :: (Coord -> Tile),
    stDir    :: Direction, -- kierunek w którym patrzył gracz podczas ostatniego ruchu
    stBoxes  :: [Coord]
}


range_n :: Int -> [Int]
range_n n = [-n..n] 
    
draw_box :: Coord -> Picture
draw_box c = translated (fromIntegral (x c)) (fromIntegral (y c)) (drawTile Box)

-- funkcje grafowe

-- foldGraph

data SearchResultType a b = SearchResultType {
    resVisited :: [a],
    resAcc :: b,
    resToVisit :: [a] -- stos rosnący w lewo 
    }

foldGraph :: Eq a => a -> (a -> [a]) -> (a -> b -> b) -> b -> b
foldGraph initial neighbours fun acc = 
    (resAcc (fold_graph_with_visited SearchResultType{resVisited = [initial], resAcc = acc, resToVisit = []} initial neighbours fun))
    
fold_graph_with_visited :: Eq a => (SearchResultType a b) -> a -> (a -> [a]) -> (a -> b -> b) -> (SearchResultType a b)

fold_graph_with_visited res initial neighbours fun = 
-- to będzie bfs
-- wrzuca jego nieodwiedzonych sąsiadów do to visit
-- i wrzuca wszystkich z kolejki do visited
-- aktualizuje acc za pomocą fun na initial 
-- wywołuje się na następnym z toVisit jeśli coś tam jest
-- jeśli stos jest pusty to zakańcza się i zwraca wynik 
    let unvisited_neighbours = (filterList (\x -> (not (elemList x (resVisited res)))) (neighbours initial)) in
    let new_to_visit = unvisited_neighbours ++ (resToVisit res) in
    let new_visited = unvisited_neighbours ++ (resVisited res) in
    let new_acc = (fun initial (resAcc res)) in (      
        if new_to_visit == []  
        then
            SearchResultType{resVisited = new_visited, resToVisit = new_to_visit, resAcc = new_acc}
        else 
            fold_graph_with_visited SearchResultType{resVisited = new_visited, resToVisit = (tail new_to_visit), resAcc = new_acc} (head new_to_visit) neighbours fun
        )
    
-- funkcje grafowe z foldem

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = foldGraph initial neighbours both_ok True
    where both_ok v acc = ((isOk v) && acc)
    
    
reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = foldGraph initial neighbours equals_or_reachable False
    where equals_or_reachable x acc = ((v == x) || acc)
    
allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList (\x -> reachable x initial neighbours) vs
    

-- funkcje pomocnicze z listami 


foldList :: (a -> b -> b) -> b -> [a] -> b
foldList fun acc [] = acc
foldList fun acc (h:t) = foldList fun (fun h acc) t


allList :: (a-> Bool) -> [a] -> Bool
allList isOk list = foldList fun True list 
    where fun element acc = ((isOk element) && acc)
    


andList :: [Bool] -> Bool
andList list = foldList (&&) True list

orList :: [Bool] -> Bool
orList list = foldList (||) False list


mapList :: (a -> b) -> [a] -> [b]
mapList fun lista = foldList (perform_and_append fun) [] lista

perform_and_append :: (a -> b) -> a -> [b] -> [b]
perform_and_append function element acc = (acc ++ [(function element)])

nth :: [a] -> Integer -> a
nth (h:t) 0 = h
nth (h:t) n = nth t (n-1)


filterList :: (a -> Bool) -> [a] -> [a]
filterList is_ok lista = foldList (add_if_ok is_ok) [] lista

add_if_ok :: (a -> Bool) -> a -> [a] -> [a]
add_if_ok is_ok element acc = 
    if (is_ok element) 
    then acc ++ [element]
    else acc
    

listLength :: [a] -> Integer
listLength [] = 0
listLength (h:t) = (listLength t) + 1

appendList :: [a] -> [a] -> [a]
appendList l1 [] = l1
appendList l1 (h2:t2) = appendList (l1 ++ [h2]) t2

elemList :: Eq a => a -> [a] -> Bool
elemList x lista = foldList (true_if_equal x) False lista 

true_if_equal :: Eq a => a -> a -> Bool -> Bool 
true_if_equal x y acc = (acc || (x == y))

-- sprawdzanie poziomów

isClosed :: Maze -> Bool 
isClosed m = 
    if ((((mazeMap m) (mazeStart m)) /= Ground) && (((mazeMap m) (mazeStart m)) /= Storage)) 
    then False
    else isGraphClosed (mazeStart m) (maze_neighbours (mazeMap m)) (\x -> not(((mazeMap m) x) == Blank))
    
maze_neighbours :: (Coord -> Tile) -> Coord -> [Coord]
maze_neighbours map c =
    filterList (\x -> ((map x) /= Wall))
    [
    (C ((x c) + 1) ((y c) + 1)),
    (C ((x c) - 1) ((y c) + 1)),
    (C ((x c) + 1) ((y c) - 1)),
    (C ((x c) - 1) ((y c) - 1))
    ]



