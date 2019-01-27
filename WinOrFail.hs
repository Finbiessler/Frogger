module WinOrFail where
import Definitions
import MovingObjects


-- A custom module containing functions which check for winning or failing conditions

checkWin :: GameState -> Bool
checkWin s = or [fx == x && fy == 1 | x <- [1..(fst dim)], (x `mod` 4 == 0)]
  where fx = (gs_pos_x s)
        fy = (gs_pos_y s)

-- checkFailure: takes the GameState and checks wether the position of the frog is a defeat criterion

checkFailure :: GameState -> Bool
checkFailure s = (dynamicFailingStates s) || (staticFailingStates s) || (isInWater s)

{-
  dynamicFailingStates:
  takes the the players gs_pos_x and gs_pos_y
  and checks if there is a moving Object in that pixle.
  If there is a moving object in that particular pixle
  the function returns True otherwise False.
-}

dynamicFailingStates :: GameState -> Bool
dynamicFailingStates s
  | checkMovingObject fx fy mos == Just Car = True
  | otherwise = False
  where fx  = gs_pos_x s
        fy  = gs_pos_y s
        mos = gs_moving_objects s

{-
  staticFailingStates:
  takes the the players gs_pos_x and gs_pos_y
  and checks if the player is on a "wrong" pixle
  on the upper display border or would leave the
  display on the right or left. If the players
  position is "wrong" it returns True otherwise False.
-}

staticFailingStates :: GameState -> Bool
staticFailingStates s = or [(fx == x && fy == 1) || fx == 0 || fx == 30 | x <- [1..(fst dim)], x `mod` 4 /= 0]
  where fx = (gs_pos_x s)
        fy = (gs_pos_y s)

{-
isInWater & checkInWater:
take the Gamestate and checks if the player is in the water or not.
Returns true if the player is in the water otherwise false.
-}
isInWater :: GameState -> Bool
isInWater s = if checkInWater s == Nothing then True else False

checkInWater :: GameState -> Maybe MovingObjectKind
checkInWater s = if fy > 1 && fy < 7 then checkMovingObject fx fy mos else Just Log
  where fx  = gs_pos_x s
        fy  = gs_pos_y s
        mos = gs_moving_objects s
