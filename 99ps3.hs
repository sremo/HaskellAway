myGcd:: Int -> Int -> Int
myGcd a b
    | a == b = a
    | a > b = myGcd (a-b) b
    | a < b = myGcd a (b-a)

myCoprime:: Int -> Int -> Bool
myCoprime a b = myGcd a b == 1

eulTot:: Int -> Int
eulTot a = length $ filter( myCoprime a) [1..a]
