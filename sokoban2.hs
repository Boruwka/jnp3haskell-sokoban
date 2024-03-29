{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
type Program = IO ()

main :: Program
main = walk3

walk1 :: IO()
walk1 = activityOf initial_state handle_event draw_state

walk2 :: IO()
walk2 = activityOf initial_state handle_event2 draw_state2

walk3 :: IO()
walk3 = resettableActivityOf initial_state handle_event2 draw_state2

resettableActivityOf :: State -> (Event -> State -> State) -> (State -> Picture) -> IO()
resettableActivityOf initial handler drawer = 
    activityOf initial (handleWithEsc handler initial) drawer
    
handleWithEsc :: (Event -> State -> State) -> State -> Event -> State -> State
handleWithEsc handler initial (KeyPress key) prev_state
    | key == "Esc" = initial
    | otherwise = handler (KeyPress key) prev_state
    
-- nic się nie zmienia po puszczeniu esc, to znaczy, że moment jego puszczenia nie ma znaczenia, tylko moment wciśnięcia
    
handleWithEsc handler _ event prev_state = handler event prev_state

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
    stMaze   :: Coord -> Tile,
    stDir    :: Direction -- kierunek w którym patrzył gracz podczas ostatniego ruchu
}

initial_state :: State
initial_state = S (C 0 1) 10 maze L

handle_event :: Event -> State -> State
handle_event (KeyPress key) state
    | key == "Right" = move_player R state
    | key == "Up"    = move_player U state
    | key == "Left"  = move_player L state
    | key == "Down"  = move_player D state
handle_event _ c      = c

handle_event2 :: Event -> State -> State
handle_event2 (KeyPress key) state
    | key == "Right" = move_player2 R state
    | key == "Up"    = move_player2 U state
    | key == "Left"  = move_player2 L state
    | key == "Down"  = move_player2 D state
handle_event2 _ c      = c

move_player :: Direction -> State -> State
move_player dir state = do {
    if (check_coord state (moveCoord dir (stPlayer state)))
    then 
        state {
            stPlayer = 
            (moveCoord dir (stPlayer state))
        }
    else 
        state
    }
    
    

move_player2 :: Direction -> State -> State
move_player2 dir state = do {
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
    
    
check_coord :: State -> Coord -> Bool -- sprawdza czy można się przemieścić na to pole
check_coord state c = do {
    if (((stMaze state c) == Ground) ||
        ((stMaze state c) == Storage))
    then True
    else False
}

draw_state :: State -> Picture
draw_state state = 
    (translated (fromIntegral (x (stPlayer state))) (fromIntegral (y (stPlayer state))) player1) &
    (pictures[ 
    draw_square state (C x y) | 
    x <- range_n (stRange state), y <- range_n (stRange state) ])
    
draw_state2 :: State -> Picture
draw_state2 state = 
    (translated (fromIntegral (x (stPlayer state))) (fromIntegral (y (stPlayer state))) (player2 (stDir state))) &
    (pictures[
    draw_square state (C x y) | 
    x <- range_n (stRange state), y <- range_n (stRange state) ])
    
   
 

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank  -- blank
  | abs x == 4 || abs y == 4 = Wall  -- wall
  | x ==  2 && y <= 0        = Wall  -- wall
  | x ==  3 && y <= 0        = Storage  -- storage
  | x >= -2 && y == 0        = Box  -- box
  | otherwise                = Ground  -- ground
  

range_n :: Int -> [Int]
range_n n = [-n..n] 


draw_square :: State -> Coord -> Picture 
draw_square state c = translated (fromIntegral (x c)) (fromIntegral (y c)) (drawTile ((stMaze state) c))

n :: Int 
n = 10

player1 :: Picture
player1 = colored yellow (
    (solidPolygon [(0.5, 0.5), (0.5, 0), (0, 0.25)]) &
    (polyline [(0.5, 0), (0.5, -0.5)]) &
    (polyline [(0.5, 0), (0, -0.5)]))
    
    
player2 :: Direction -> Picture 
player2 R = rotated 3.14 player1
player2 U = rotated 4.71 player1
player2 L = player1 
player2 D = rotated 1.57 player1
