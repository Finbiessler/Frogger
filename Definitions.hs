module Definitions where
{-
A custom module containing definitions of important types and constants.
Since these are referenced at a lot of places across our code it
seemed appropriate to collect them here.
-}

-- Necessary Modules.
import Network.MateLight.Simple


{-
The definition of the GameState, which is updated with every frame.
The GameState is a Record Type consisting of the following members:
    1)  A frame counter, increasing with every frame.
        This is useful for establishing a notion of time.
    2)  The x- and y-position of the player character, a.k.a. the frog.
    3)  A list containing all moving objects present in the game right now.
    4)  A list containing counters for each row.
        A 3-tuple as it contains
            a) The row-number itself.
            b) The current counter that increases every frame.
            c) The goal for the counter to reach, randomly set.
-}
data GameState = GameState {
        gs_frame_count      :: Int ,
        gs_pos_x            :: Int ,
        gs_pos_y            :: Int ,
        gs_moving_objects   :: [MovingObject] ,
        gs_row_counters     :: [(Int, Int, Int)],
        gs_points_count     :: [Int]
    } deriving Eq

--gs_pos_x :: GameState -> Int
--gs_pos_x (GameState _ x _ _ _) = x



-- We need to definie a starting state that will be passed to runMateRandom.
-- It is defined here.
startingState :: GameState
startingState = GameState
                            -- Frame counter starts at 0.
                            0
                            -- The frog starts at (0,0) too, this will
                            -- probably change in the future.
                            15
                            12
                            -- At the beginning of the game there are no
                            -- moving objects.
                            []
                            -- All row counters are initially zero.
                            [(i, 0, 0) | i <- [2..6] ++ [8..11]]
                            [0 | x <- [0..12]]


-- This datatype represents the different kinds of movingObjects.
data MovingObjectKind = Car | Log deriving Eq

-- This datatype represents a moving object.
-- MovingObjects ...
--  1) ... are of a certain kind.
--  2) ... have a position (x and y).
--  3) ... have a direction in which they move (left (-1) or right (1))
--  4) ... are a certain amount of pixels long.
--  5) ... move at a certain speed. (i.e. every 6th frame)
--  6) ... are represented by a screen color.
data MovingObject = MovingObject {
        mo_kind             :: MovingObjectKind,
        mo_x                :: Int,
        mo_y                :: Int,
        mo_direction        :: Int,
        mo_length           :: Int,
        mo_speed            :: Int,
        mo_color            :: Pixel
    }deriving Eq

-- This is a list of tuples, defining the spawning behaviour for each row.
-- This allows us to define the parameters of the moving objects
-- individually for every row.
-- These parameters are:
--  1) The row. (given as a y-coordinate)
--  2) The kind of MovingObject to spawn.
--  3) Its direction.
--  4) Its length.
--  5) Its speed.
--  6) Its spawn window.
--      For instance, a value of 59 means that it will take
--      between 0 and 59 frames to spawn a new object, each of these frames being
--      equally likely. (a.k.a. uniformly distributed.)
--      In other words, a new object could spawn at the earliest immediately or
--          at the latest after two seconds (60 frames).
--  7) Its color.
movingObjectSpawnData :: [(Int,MovingObjectKind, Int, Int, Int, Int, Pixel)]
movingObjectSpawnData =
    [
    --  Row,    Kind,   Dir,    Length, Speed,  Window, Color
       (2,      Log,    1,      4,      9,      90,    (Pixel 0xB0 0x80 0x00)),
       (3,      Log,    1,      6,      2,      40,     (Pixel 0xB0 0x80 0x00)),
       (4,      Log,    (-1),   3,      10,     80,    (Pixel 0xB0 0x80 0x00)),
       (5,      Log,    1,      5,      3,      60,    (Pixel 0xB0 0x80 0x00)),
       (6,      Log,    (-1),   4,      14,     70,    (Pixel 0xB0 0x80 0x00)),

       (8,      Car,    (-1),   4,      5,      60,    (Pixel 0xFF 0xFF 0x00)),
       (9,      Car,    1,      2,      10,     50,    (Pixel 0xFF 0x00 0x00)),
       (10,     Car,    (-1),   3,      6,      42,    (Pixel 0xFF 0xFF 0x00)),
       (11,     Car,    1,      4,      12,     60,     (Pixel 0xFF 0x00 0x00))
    ]

-- This constant defines the screen-resolution.
dim :: (Int, Int)
dim = (30, 12)

-- This constant defines the delay between every frame in ms.
-- A delay of 33000 results in roughly 30 fps.
frameDelay :: Int
frameDelay = 33000
