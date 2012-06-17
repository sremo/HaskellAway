
import Control.Monad
import Data.Maybe (maybeToList)

type Father = Maybe Sheep
type Mother = Maybe Sheep
type SheepName = String
-- data Sheep = SexualReproduction SheepName Mother Father | ClonedFromFather SheepName Father  | ClonedFromMother SheepName Father | FirstSheep SheepName deriving (Show, Read)
data Sheep = Sheep SheepName Mother Father

-- father:: Sheep -> Maybe Sheep
-- father (ClonedFromMother _ _) = Nothing
-- father (ClonedFromFather shpName ftSheep) = Just ftSheep
-- father (SexualReproduction shpName mtSheep ftSheep) = Just ftSheep
-- father (FirstSheep _) = Nothing


-- mother:: Sheep -> Maybe Sheep
-- mother (ClonedFromFather _ _) = Nothing
-- mother (ClonedFromMother shpName mtSheep) = Just mtSheep
-- mother (SexualReproduction shpName mtSheep ftSheep) = Just mtSheep
-- mother (FirstSheep _) = Nothing

-- maternalGrandmother :: Sheep -> Maybe Sheep
-- maternalGrandmother s = case (mother s) of
--                         Nothing -> Nothing
--                         Just mt -> mother mt

comb :: Maybe a -> (a -> Maybe b) -> Maybe b 
comb Nothing _ = Nothing
comb (Just x) f = f x

--maternalGrandmotherC s = (Just s) `comb` mother `comb` mother

-- maternalGranmotherM s = (return s) >>= mother >>= xmother
-- paternalGrandmotherM s = (return s) >>= father >>= mother


-- parent :: Sheep -> Maybe Sheep
-- parent s = mother s `mplus` father s

-- grandParent s = (return s) >>= parent >>= parent


--mpfather (ClonedFromMother _ _) = mzero 
-- mpfather (ClonedFromFather shpName ftSheep) = return ftSheep
-- mpfather (SexualReproduction shpName mtSheep ftSheep) = return ftSheep
-- mpfather (FirstSheep _) = mzero

-- mpmother:: (MonadPlus m) => Sheep -> m Sheep
-- mpmother (ClonedFromFather _ _) = mzero
-- mpmother (ClonedFromMother shpName mtSheep) = return mtSheep
-- mpmother (SexualReproduction shpName mtSheep ftSheep) = return mtSheep
-- mpmother (FirstSheep _) = mzero

--mpfather = father
--mpmother = mother


mpfather:: (MonadPlus m) => Sheep -> m Sheep
mpfather (Sheep sheepName fatherSheep motherSheep) = fatherSheep

mpmother:: (MonadPlus m) => Sheep -> m Sheep
mpmother (Sheep sheepName fatherSheep motherSheep) = motherSheep


parent :: (MonadPlus m) => Sheep -> m Sheep
parent s = (mpfather s) `mplus` (mpmother s)

grandParent :: (MonadPlus m) => Sheep -> m Sheep
grandParent s = (parent s) >>= parent