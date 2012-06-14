import System.Environment (getArgs)


interactWith funct inputFile outputFile = do
	input <- readFile inputFile
	writeFile outputFile (funct input)

main = mainWith myFunct
	where mainWith funct = do
		args <- getArgs
		case args of
			[input,output] -> interactWith funct input output
			_ -> error "Exactly two arguments needed"
	      myFunct = id



