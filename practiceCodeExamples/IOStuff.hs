--main = do 
--	putStrLn "Hey! Whaddup dawg?"
--	name <- getLine
--	putStrLn $ "Ok, bye " ++ name ++ " dawg"


--main = putStrLn "Hey! " >> getLine >>= (\name -> putStrLn $ "aho " ++ name ++ " !")

import Data.Char
import Control.Monad
import System.IO
import Data.List
import System.Directory

upName = putStrLn "What is your first name?" >> getLine >>= f
	where f first = putStrLn "What is your last name?" >>
		getLine >>= g
		where g last = putStrLn $ map toUpper $ first ++ " " ++ last

-- (\(inpA, inpB) -> putStrLn $ inpA ++ inpB )

reverseWordsTypedText = do inp <- getLine
		      	   if null inp
					then return ()
					else do putStrLn $ unwords $ map reverse $ words inp
				        	reverseWordsTypedText 
	
myPutStrLn [] = putChar '\n'  >> return ()
myPutStrLn (x:xs) = putChar x >> myPutStrLn xs

myPrint:: (Show a) => a -> IO ()
myPrint = myPutStrLn . show

toUpChar = getChar >>= (\chInp ->  putStr $ "\n" ++ [toUpper chInp] ++ "\n" )		
reOutput = do a <- getChar 
	      if (a /= ' ') 
		 then do
			putChar a 
			reOutput
		 else return () 

reOutputWhen = getChar >>= (\a -> when (a/= ' ') (putChar a >> reOutputWhen) )

tryOutSeq = do  a <- sequence [getLine, getLine]
		print a

--tryOutSeq2 = do a <- [getLine, getLine]
--		print a

-- try equivalence of forM and sequence
tryThisSeq = sequence $ map (\a -> print a) [1,2,3,4] 

tryThisMapM = mapM_ (\a -> print a) [1,2,3,4]


capslockerF = forever $ do
		l <- getLine
		putStrLn $ unwords $ (map reverse) $ words l
 

capslockerG = do 
		l <- getContents
		putStrLn $ unlines $ (map reverse) $ lines l	

tryInteract = interact $ (\a -> unwords $ map reverse (words a))


fileInteraction = do
		handle <- openFile "testFile.txt" ReadMode
		cont <- hGetContents handle
		putStr cont
		hClose handle

fileInteractionWF = withFile "testFile.txt" ReadMode (\h -> hGetContents h >>= (\cont -> putStr cont))

myWithFile:: FilePath -> IOMode -> (Handle -> IO a) -> IO a
myWithFile path iom f = do 
			hndl <- openFile path iom
			rs <- f hndl
			hClose hndl
			return rs
			--hClose hndl

fileInteractionMyWF = myWithFile "testFile.txt" ReadWriteMode fnc

fnc h = do cont <- hGetContents h 
           --hPutStr h $ map toUpper cont
	   putStr "remo"
--fileInteractionMyWF = myWithFile "testFile.txt" ReadWriteMode (\h -> (hGetContents h,h) >>= (\inp -> hPutStr $ fst inp $ map toUpper (snd inp)))


forevaApnd = forever $ do
		nwLine <- getLine
		appendFile "td.txt" nwLine

listItems:: [String] -> IO ()
listItems [fileNm] = do 
			cont <- readFile fileNm
		   	let numberedTasks = zipWith (\n taskLine -> show n ++ " " ++ taskLine) [0..] $ lines cont
		   	mapM_ putStrLn numberedTasks

addItem:: [String] -> IO ()
addItem (fileNm:(taskString:[])) = do appendFile fileNm $ taskString ++ "\n"


removeItem:: [String] -> IO ()
removeItem (fileNm:(itemNo:[])) = do cont <- readFile fileNm
			      	     if (length $ lines cont)  <=  (read itemNo)
			      	     then return ()
			             else do (tempName, tempHandle) <- openTempFile "." "temp"
				             let newTodoList = delete ((lines cont) !! (read itemNo)) (lines cont)
				             hPutStr tempHandle $ unlines newTodoList
				             hClose tempHandle
				             removeFile fileNm
				             renameFile tempName fileNm 

todoListManager:: [String] -> IO ()
todoListManager (funcNm:parList) 
			| funcNm  == "listItems" = listItems parList --(head parList)
			| funcNm == "addItem" = addItem parList
			| funcNm == "removeItem" = removeItem parList
				      

dispatch:: [(String, [String] -> IO ())]
dispatch = [("listItems",listItems), ("addItem",addItem),("removeItem",removeItem)]

todoListManagerDispatch (command:parList) = do let (Just action) = lookup command dispatch 
					       action parList
