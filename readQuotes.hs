import System.IO  
import Numeric.LinearAlgebra
import Numeric
  
disp = putStr . dispf 2
dispv = putStr . vecdisp (disps 2)

main = do  
    contents <- readFile "quotes.txt"  
    let vec = map read (words contents)
    --let vec = (fromList $ ( map lexDigits (words $ contents) )) :: Vector Double
    -- show vec
    putStrLn contents
    --dispv vec
    
