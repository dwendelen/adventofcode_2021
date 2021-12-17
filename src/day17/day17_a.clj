; Observation: When coming down, it follows the same Y-trajectory back down
; Max velocity, is just-not-missing target area - 1 -> because initial velocity is previous
;     step. just-not-missing step = initial + 1
(def minY 90)
(def maxInitialYVelocity (dec minY))
(def maxHeight (/ (* maxInitialYVelocity (inc maxInitialYVelocity)) 2))
(println maxHeight)