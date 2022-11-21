{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
type Program = IO ()

main :: Program
main = walk

--main = drawingOf (pictureOfBoxes initialBoxes)

walk :: IO()
--walk = resettableActivityOf initial_state handleEvent draw
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
startScreen = lettering("Witamy w Sokobanie!")


-- pierwotny stan 

initial_state :: State
initial_state = S (C 0 1) 10 initial_maze initial_maze L initialBoxes

initial_maze :: Maze
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

addBoxes :: [Coord] -> Maze -> Maze
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
    
-- podstawowe definicje

allList :: [Bool] -> Bool 
allList list = foldl (&&) True list

isWinning :: State -> Bool 
isWinning state = allList (map (is_on_storage state) (stBoxes state))
        
is_on_storage :: State -> Coord -> Bool
is_on_storage state c = ((stInitialMaze state) c == Storage)
    

type Maze = Coord -> Tile

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
    stMaze   :: Maze,
    stInitialMaze :: Maze,
    stDir    :: Direction, -- kierunek w którym patrzył gracz podczas ostatniego ruchu
    stBoxes  :: [Coord]
}


range_n :: Int -> [Int]
range_n n = [-n..n] 
    
draw_box :: Coord -> Picture
draw_box c = translated (fromIntegral (x c)) (fromIntegral (y c)) (drawTile Box)

