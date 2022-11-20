{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
type Program = IO ()

main :: Program
main = walk

--main = drawingOf (pictureOfBoxes initialBoxes)

walk :: IO()
walk = resettableActivityOf initial_state handle_event draw_state

-- resetowanie gry

resettableActivityOf :: State -> (Event -> State -> State) -> (State -> Picture) -> IO()
resettableActivityOf initial handler drawer = 
    activityOf initial (handleWithEsc handler initial) drawer
    
handleWithEsc :: (Event -> State -> State) -> State -> Event -> State -> State
handleWithEsc handler initial (KeyPress key) prev_state
    | key == "Esc" = initial
    | otherwise = handler (KeyPress key) prev_state
       
handleWithEsc handler _ event prev_state = handler event prev_state


-- pierwotny stan 

initial_state :: State
initial_state = S (C 0 1) 10 initial_maze L initialBoxes

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

handle_event :: Event -> State -> State
handle_event (KeyPress key) state
    | key == "Right" = move_player R state
    | key == "Up"    = move_player U state
    | key == "Left"  = move_player L state
    | key == "Down"  = move_player D state
handle_event _ c      = c
  

move_player :: Direction -> State -> State
move_player dir state = do {
    if (check_coord state (moveCoord dir (stPlayer state)))
    then 
        state {
            stPlayer = 
            (moveCoord dir (stPlayer state)),
            stDir = dir
        }
    else 
        state
    }
    

-- funkcja rysująca
    
draw_state :: State -> Picture
draw_state state = 
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
    
-- podstawowe definicje

type Maze = Coord -> Tile

getBoxes :: (Coord -> Tile) -> [Coord]
getBoxes maze = [(C x y) | x <- range_n n, y <- range_n n, maze (C x y) == Box]

removeBoxes :: (Coord -> Tile) -> Coord -> Tile
removeBoxes maze =  fun . maze
  where
         fun Box = Ground
         fun tile = tile

data Direction = R | U | L | D
data Coord = C {x :: Int, y :: Int}

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
check_coord state c = do {
    if (((stMaze state c) == Ground) ||
        ((stMaze state c) == Storage))
    then True
    else False
} 

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
    stDir    :: Direction, -- kierunek w którym patrzył gracz podczas ostatniego ruchu
    stBoxes  :: [Coord]
}


range_n :: Int -> [Int]
range_n n = [-n..n] 

pictureOfBoxes :: [Coord] -> Picture
pictureOfBoxes boxes = 
    (pictures[
    draw_box c | c <- boxes])
    
draw_box :: Coord -> Picture
draw_box c = translated (fromIntegral (x c)) (fromIntegral (y c)) (drawTile Box)

