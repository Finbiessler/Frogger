module Main where

{-
This module contains all the top-level functions of our game.
This includes among others the main-function as well as the function
called every frame (advanceFrame).
-}

-- First, import the necessary modules.
import Network.MateLight.Simple
import Data.Maybe
import qualified Network.Socket as Sock
import Debug.Trace

-- Then, import our own modules.
import Definitions
import MovingObjects
import FeedbackFinished
import WinOrFail
import PointShit


-- This functions returns the requested Pixel of the background image.
background :: Int -> Int -> Pixel
background x y      | y == 7 || y == 12              = Pixel 0x80 0x00 0x80
                    | y > 1  && y < 7                = Pixel 0x00 0x30 0x60
                    | y == 1 &&  x `mod` 4 /= 0      = Pixel 0x00 0x80 0x00
                    | otherwise                      = Pixel 0x00 0x00 0x00

-- This function renders a frame from a GameState.
-- It iterates through all Pixels of the screen to calculate their Pixel values.
-- The color of the Pixel is then determined by the helper function, checking
-- different conditions in a certain order. (see below)
renderFrame :: GameState -> ListFrame
renderFrame s = ListFrame [ [ helper x y s | x <- [1..(fst dim)]] | y <- [1..(snd dim)]]

    where
        --shows the points
        helper 30 y s | ((gs_points_count s) !! (y - 1)) == 1     = Pixel 0xFF 0xFF 0xFF
        helper x _ _  | x == 30                                   = Pixel 0x00 0x00 0x00
        -- Checks wether the position of the frog is on the topmost line
        --if so, the function renders displayWon.
        helper x y s | checkFailure s                             = displayLost x y
        -- Checks wether the position of the frog is a defeat criterion
        --if so, the function renders displayLost.
        helper x y s | checkWin s                                 = displayWon x y
        -- displays the frog
                     | (x == (gs_pos_x s)) &&  (y == (gs_pos_y s))  = Pixel 0x00 0xFF 0x00
        -- Check for a movingObject. If there is one, draw it in red.
                     | isJust (pixelOfMovingObject x y (gs_moving_objects s))
                        = fromJust (pixelOfMovingObject x y (gs_moving_objects s))
        -- Lastly, if no other condition was met, draw the background.

                     | otherwise                            = background x y

-- This function evaluates the input events relating to the frog's movement.
-- Takes the current positions (x,y) and the list of events and returns
-- the new position (x,y).
checkKey :: (Int, Int) -> [Event String] -> (Int, Int)
checkKey (x, y) es =   (if helper "\"a\"" es then x-1 else if helper "\"d\"" es then x+1 else x,
                        if helper "\"w\"" es then y-1 else if helper "\"s\"" es then y+1 else y)
    where helper s es = (Event "KEYBOARD" s) `elem` es

-- This function updates the position of the frog.
-- First, it applies logMovement, if applicable.
-- Then, it calculates the result of keyboard inputs.
-- Returns the position as a 2-tuple (x,y).
updateFrog :: [Event String] -> GameState -> (Int, Int)
updateFrog e s = checkKey (applyLogMovement (gs_pos_x s, gs_pos_y s) s) e

-- This function updates the game logic and computes a new GameState.
-- To do so it receives:
--  1) The random input.
--  2) The list of events, containing the keyboard input.
--  3) The old GameState.
update :: [Int] -> [Event String] -> GameState -> GameState
update r e s
              | checkFailure s = startingState
              | checkWin s = startingState
              | otherwise = GameState
                        -- Advance the frame counter by 1.
                        (gs_frame_count s + 1)
                        -- Frog position remains unchanged for now.
                        -- This would be a good place to handle keyboard input
                        -- for the frog.
                        (fst (updateFrog e s))
                        (snd (updateFrog e s))
                        -- Call the updater for the moving objects.
                        (updateMovingObjects s)
                        -- Also update the row counters.
                        (iterateRowCounters r (gs_row_counters s))
                        (pointItUp (gs_points_count s) e)


-- advanceFrame is the main loop of our program.
-- Called every frame, it receives
--  1) random input [Int]
--  2) a list of events (keyboard input) [Event String]
--  3) and the current state. (GameState)
-- It returns the new GameState together with the frame (ListFrame) to be drawn.
advanceFrame :: [Int] -> [Event String] -> GameState -> (ListFrame, GameState)

-- Call the renderer and updater respectively.
advanceFrame r e s = (renderFrame s, update r e s)

-- Lastly, the actual main-function.
main :: IO ()
main = Sock.withSocketsDo $ runMateRandom (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just frameDelay) False []) advanceFrame startingState
