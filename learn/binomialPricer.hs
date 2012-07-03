

splitPrices:: (Floating a) => a -> a  -> [a]
splitPrices S u = [u, 1/u]

valueOptionCall Str Sp
                | Str < Sp = Sp -Str
                | otherwise = 0