module WinningStates where
import Definitions

--custom module containing definitions of winning states which will be used to compare thmem with the current "GameState"
--to see if if the game is finished yet

--Position x = 0 & y = 0
winningState00 :: GameState
winningState00 = GameState 0 0 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 1 & y = 0
winningState01 :: GameState
winningState01 = GameState 0 1 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 2 & y = 0
winningState02 :: GameState
winningState02 = GameState 0 2 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 4 & y = 0
winningState04 :: GameState
winningState04 = GameState 0 4 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 5 & y = 0
winningState05 :: GameState
winningState05 = GameState 0 5 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 6 & y = 0
winningState06 :: GameState
winningState06 = GameState 0 6 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 8 & y = 0
winningState08 :: GameState
winningState08 = GameState 0 8 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 9 & y = 0
winningState09 :: GameState
winningState09 = GameState 0 9 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 10 & y = 0
winningState10 :: GameState
winningState10 = GameState 0 10 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 12 & y = 0
winningState12 :: GameState
winningState12 = GameState 0 12 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 13 & y = 0
winningState13 :: GameState
winningState13 = GameState 0 13 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 14 & y = 0
winningState14 :: GameState
winningState14 = GameState 0 14 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 16 & y = 0
winningState16 :: GameState
winningState16 = GameState 0 16 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 17 & y = 0
winningState17 :: GameState
winningState17 = GameState 0 17 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 18 & y = 0
winningState18 :: GameState
winningState18 = GameState 0 18 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 20 & y = 0
winningState20 :: GameState
winningState20 = GameState 0 20 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 21 & y = 0
winningState21 :: GameState
winningState21 = GameState 0 21 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 22 & y = 0
winningState22 :: GameState
winningState22 = GameState 0 22 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 24 & y = 0
winningState24 :: GameState
winningState24 = GameState 0 24 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 25 & y = 0
winningState25 :: GameState
winningState25 = GameState 0 25 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 26 & y = 0
winningState26 :: GameState
winningState26 = GameState 0 26 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 28 & y = 0
winningState28 :: GameState
winningState28 = GameState 0 28 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]

--Position x = 29 & y = 0
winningState29 :: GameState
winningState29 = GameState 0 29 0 [] [(i, 0, 0) | i <- [2..6] ++ [8..11]]
