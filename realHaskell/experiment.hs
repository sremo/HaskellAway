

main = do
	putStrLn "Read quotes and compute average"
	quotes <- readFile "quotes.txt"
	let prices = map (\l -> read l::Double) (words quotes)
	let a = (sum prices) / (read $ show $ length prices::Double)
	putStrLn $ "This is the average price " ++ (show a)

