module PointShit where

import Definitions
import Network.MateLight.Simple

--Binary adder
binAdd :: [Int] -> [Int]
binAdd [] = [1]
binAdd (1:xs) = [0] ++ binAdd xs
binAdd (0:xs) = [1] ++ xs

-- Takes points, EventString and adds + 1 to Points in binary if w is pressed
pointItUp :: [Int] -> [Event String] -> [Int]
pointItUp x e | (Event "KEYBOARD" "\"w\"") `elem` e        = binAdd x
pointItUp x _ | otherwise = x




