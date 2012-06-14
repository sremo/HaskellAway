import System.Random

randomSequence randomStdGen = rndNumber:randomSequence nxtStdGen
			      where (rndNumber,nxtStdGen) = random randomStdGen

initRandomSequence seedInt = randomSequence $ mkStdGen seedInt

finiteRndNumSeq:: (RandomGen g, Random a, Num n) => g -> n -> ([a],g)
finiteRndNumSeq gen 0 = ([],gen)
finiteRndNumSeq gen n = let (val,nxtGen) = random gen
			    (restOfList, finGen) = finiteRndNumSeq nxtGen $ n-1
			in (val:restOfList,finGen)
