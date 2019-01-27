module MovingObjects where

{-
This module contains all the functions for the moving objects in the game.
This includes the cars on the street and the logs in the river.
It aims to provide different functions useful for different parts of the game.
-}

-- Import necessary definitions and constants.
import Definitions
import Data.Maybe
import Network.MateLight.Simple

-- This function returns a list of all movingObjects that occupy the given pixel.
-- Its meant to be used by other MovingObject functions and is not be called directly
-- from Main.hs, but then again, nothing will stop you.
findMovingObjects :: Int -> Int -> [MovingObject] -> [MovingObject]
findMovingObjects x y mos = [o
                            | o <- mos
                            , (mo_y o) == y
                            , x >= (mo_x o) && x < (mo_x o) + (mo_length o)
                            ]

-- This function checks whether there is a moving object present in the requested pixel.
-- Returns Nothing if there is no movingObject on the requested pixel.
-- Returns Just Car, Just Log, etc... if there is a MovingObject present.
-- Arguments: x-Pos, y-Pos, list of MovingObjects to check
checkMovingObject :: Int -> Int -> [MovingObject] -> Maybe MovingObjectKind
checkMovingObject x y mos = helper (findMovingObjects x y mos)
    where   helper []       = Nothing
            helper (x:xs)   = Just (mo_kind x)

-- This function returns the color of the movingObject in the requested pixel.
-- Returns Nothing if there is no MovingObject present in the requested pixel.
-- Takes the x- and y-position as well as a list of all MovingObjects to check.
pixelOfMovingObject :: Int -> Int -> [MovingObject] -> Maybe Pixel
pixelOfMovingObject x y mos = helper (findMovingObjects x y mos)
    where   helper []       = Nothing
            helper (x:xs)   = Just (mo_color x)

-- This function changes the given (x,y)-coordinates if they are currently occupied by a log.
-- This should be used to move the frog should it be standing on a log.
applyLogMovement :: (Int, Int) -> GameState -> (Int, Int)
-- Check whether the player is on a log.
applyLogMovement (x,y) s =
    if checkMovingObject x y (gs_moving_objects s) == Just Log
        -- Then retrieve that specific log and handle the calculation in a helper.
        then (helper x s (head (findMovingObjects x y (gs_moving_objects s))), y)
        else (x,y)
    where helper x s mo = if (gs_frame_count s) `mod` (mo_speed mo) == 0 then x + (mo_direction mo) else x

-- This function updates all moving objects.
-- This includes moving them, removing those that have exited the screen
-- but also spawning new ones when necessary.
updateMovingObjects :: GameState -> [MovingObject]
updateMovingObjects s = 
    -- Update all objects in every frame here, the movingObjectsUpdateInterval
    -- will be applied at a lower level.
    --
    -- First, update all objects.
    -- Then, throw out those that are out-of-bounds.
    -- Lastly, add new ones, if necessary.
    (filter isMovingObjectInBounds [updateSingleMovingObject o s | o <- gs_moving_objects s]) ++
    addMovingObjects s (filter isMovingObjectInBounds [updateSingleMovingObject o s | o <- gs_moving_objects s])
        
-- Updates a single moving object.
-- This usually means moving it across the screen in some way.
updateSingleMovingObject :: MovingObject -> GameState -> MovingObject
updateSingleMovingObject o s = 
    -- Update only those MovingObjects that are supposed to move.
    if (gs_frame_count s) `mod` (mo_speed o) == 0
        then MovingObject
            (mo_kind o)
            ((mo_x o) + (mo_direction o))
            (mo_y o)
            (mo_direction o)
            (mo_length o)
            (mo_speed o)
            (mo_color o)
        else o

-- This function checks whether a movingObject is still in bounds.
isMovingObjectInBounds :: MovingObject -> Bool
isMovingObjectInBounds o =
    if ((mo_x o) <= (-(mo_length o))) || ((mo_x o) > fst dim)
        then False else True

-- This function generates new MovingObjects.
-- Takes as parameters:
--  1) The current GameState.
--  2) The updated list of current MovingObjects. [MovingObject]
addMovingObjects :: GameState -> [MovingObject] -> [MovingObject]
addMovingObjects s mos = concat [spawnRow s mos rowData | rowData <- movingObjectSpawnData]
    -- Iterate through all rows.
    -- Check whether an object already blocks the way in the current row.
    where spawnRow s mos (row, kind, dir, length, speed, chance, color) = if all isNothing [checkMovingObject c row mos | c <- (if dir == 1 then [(1-length)..1] else [((fst dim)-1)..((fst dim)+length-1)]) ] then
                -- If the way is clear, check the RNG-counter.
                if checkRowCounterOverflow row (gs_row_counters s)
                    -- Then actually spawn a new object.
                    then [  MovingObject
                                        kind
                                        (if dir == 1 then 1-length else (fst dim))
                                        row
                                        dir
                                        length
                                        speed
                                        color   
                         ]
                    else [] 
            else [] 

-- Iterate upon the row-counters.
-- Args:
--  1) The random input.
--  2) The old row counters.
iterateRowCounters :: [Int] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
-- This function calculates the new state of the row_counters.
-- Generally, this simple means incrementing the counter (the 2nd value of the tuple).
-- However, if an overflow occurs, a new object will be spawned by the movingObjectsUpdater
-- and the counter needs to be reset.
-- The counter will be reset to a negative value to account for the time where the object
-- slowly fades into the screen. (Otherwise the distribution wouldn't be uniform.)
-- Then, the new counter-goal (the 3rd value of the tuple) will be set to a new random value.
iterateRowCounters rand rc = [if lcount >= lgoal then (lrow, getStartVal lrow, (rand!!lrow `mod` ((getChance lrow) - 1))) else (lrow, lcount+1, lgoal) | (lrow, lcount, lgoal) <- rc]
    where getChance row = head [c | (lrow, _, _, _, _, c, _) <- movingObjectSpawnData, lrow == row]
          getStartVal row = head [-((length+1)*speed) | (lrow, _, _, length, speed, _, _) <- movingObjectSpawnData, lrow == row]
          
-- Check whether a row counter experienced an overflow.
checkRowCounterOverflow :: Int -> [(Int, Int, Int)] -> Bool
checkRowCounterOverflow row rc = or [lcount >= lgoal | (lrow, lcount, lgoal) <- rc, lrow == row]
