import System.IO



reply2name :: String -> String
reply2name nm = 
	"Hey What's up?" ++ nm ++"\n"++
	"Are you ready for some party?"


testIOFunc1 = do
--main = do
	inp <- getLine
	putStrLn $ reply2name inp
 

--permutations :: [a] ->[[a]]
--permutations [] = []
--permutations (x:xs) =


--testFileIO = do
main = do
       inF <- openFile "input.txt" ReadMode
       oF <- openFile "output.txt" WriteMode
       actOnText inF oF
       hClose inF
       hClose oF
       printF <- openFile "output.txt" ReadMode
       printTextInFile printF
       hClose printF

printTextInFile :: Handle -> IO ()
printTextInFile inpHndl = do
	isEoF <- hIsEOF inpHndl
	if isEoF
		then return ()
		else do rawTxt <- hGetLine inpHndl
			--let processedTxt = process rawTxt
			putStrLn rawTxt
			printTextInFile inpHndl
			--where process = intercal.words
			--	where intercal (x:xs) = (x++"\n")++(intercal xs)
			--	      intercal [] = []

actOnText :: Handle -> Handle -> IO ()
actOnText inHndl oHndl = do 
    isEoF <- hIsEOF inHndl
    if isEoF
        then return ()
        else do rawTxt <- hGetLine inHndl
                --filAdr <- hTell inHndl
                --putStrLn (show filAdr)
                let processedTxt = process rawTxt
                hPutStrLn oHndl processedTxt
                actOnText inHndl oHndl
                where process = intercal.words
                        where intercal (x:xs) = (x++"\n")++(intercal xs)
                              intercal [] = []



--process ao = intercal $ words ao
--	        where intercal (x:xs) = (x++"\n")++ (intercal xs)
--		      intercal [] = []
   
    
