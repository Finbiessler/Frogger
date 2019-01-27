module FeedbackFinished where
import Network.MateLight.Simple



-- displays "YOU WON"
displayWon x y
                      | y == 3 && (x == 1 || x == 3 || x == 5 || x == 6 || x == 7
                      || x == 9 || x == 11 || x == 15 || x == 17 || x == 19
                      || x == 21 || x == 22 || x == 23 || x == 25 || x == 26 || x == 27)      = Pixel 0xFF 0xFF 0xFF
                      | y == 4 && (x == 1 || x == 3 || x == 5 || x == 7 || x == 9
                      || x == 11 || x == 15 || x == 17 || x == 19 || x == 21 || x == 23
                      || x == 25 || x == 27)                                                  = Pixel 0xFF 0xFF 0xFF
                      | y == 5 && (x == 1 || x == 3 || x == 5 || x == 7 || x == 9
                      || x == 11 || x == 15 || x == 17 || x == 19 || x == 21 || x == 23
                      || x == 25 || x == 27)                                                  = Pixel 0xFF 0xFF 0xFF
                      | (y == 6 || y == 7 || y == 8) && (x == 2 || x == 5 || x == 7 || x == 9
                      || x == 11
                      || x == 15 || x == 17 || x == 19 || x == 21 || x == 23 || x == 25
                      || x == 27)                                                             = Pixel 0xFF 0xFF 0xFF
                      | y == 9 && (x == 2 || x == 5 || x == 6 || x == 7 || x == 9 || x == 10
                      || x == 11 || x == 15 || x == 16 || x == 18 || x == 19  || x == 21
                      || x == 22 || x == 23 || x == 25  || x == 27)                           = Pixel 0xFF 0xFF 0xFF
                      | otherwise                                                             = Pixel 0x00 0x00 0x00

-- displays "YOU LOST"
displayLost x y
                      | y == 3 && (x == 1 || x == 3 || x == 5 || x == 6 || x == 7
                      || x == 9 || x == 11 || x == 15 || x == 18 || x == 19
                      || x == 20 || x == 22 || x == 23 || x == 24 || x == 26 || x == 27
                      || x == 28)                                                              = Pixel 0xFF 0xFF 0xFF
                      | y == 4 && (x == 1 || x == 3 || x == 5 || x == 7 || x == 9
                      || x == 11 || x == 15 || x == 18 || x == 20 || x == 22 || x == 27)       = Pixel 0xFF 0xFF 0xFF
                      | y == 5 && (x == 1 || x == 3 || x == 5 || x == 7 || x == 9
                      || x == 11 || x == 15 || x == 18 || x == 20 || x == 22 || x == 23
                      || x == 24 || x == 27)                                                   = Pixel 0xFF 0xFF 0xFF
                      | y == 6 && (x == 2  || x == 5 || x == 7 || x == 9 || x == 11 || x == 15
                      || x == 18 || x == 20 || x == 22 || x == 23 || x == 24 || x == 27)       = Pixel 0xFF 0xFF 0xFF
                      | (y == 7 || y == 8) && (x == 2 || x == 5 || x == 7 || x == 9
                      || x == 11 || x == 15 || x == 18 || x == 20 || x == 24 || x == 27)       = Pixel 0xFF 0xFF 0xFF
                      | y == 9 && (x == 2 || x == 5 || x == 6 || x == 7 || x == 9
                      || x == 10 || x == 11 || x == 15 || x == 16 || x == 18 || x == 19
                      || x == 20 || x == 22 || x == 23 || x == 24 || x == 27)                  = Pixel 0xFF 0xFF 0xFF
                      | otherwise                                                              = Pixel 0x00 0x00 0x00
