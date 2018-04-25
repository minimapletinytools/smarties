--{-# LANGUAGE TypeSynonymInstances           #-}

module Main where


import Smarties2
import System.Random
import Control.Monad.Random hiding (sequence)
import Prelude hiding (sequence)
import Data.List (mapAccumL)

{-


data Pronoun = HeHim | SheHer | TheyThem | FooBar | Other | Undecided deriving (Eq, Show)

data Student = Student {
    assignedPronoun :: Pronoun,
    preferredPronoun :: Pronoun,
    openlyChange :: Bool,
    jeans :: Int
} deriving (Show)

assignedPronounIs :: Pronoun -> Student -> Bool
assignedPronounIs p s = preferredPronoun s == p

preferredPronounIs :: Pronoun -> Student -> Bool
preferredPronounIs p s = preferredPronoun s == p

feminimity :: Student -> Float
feminimity = fst . randomR (0.0,100.0) . mkStdGen . (+0) . jeans

masculinity :: Student -> Float
masculinity = fst . randomR (0.0,100.0) . mkStdGen . (+1) . jeans

chromeXX :: Student -> Bool
chromeXX = (<50) . fst . randomR ((0,100)::(Int,Int)) . mkStdGen . (+2) . jeans

chromeXY :: Student -> Bool
chromeXY = (>50) . fst . randomR ((0,100)::(Int,Int)) . mkStdGen . (+2) . jeans

chromeNeither :: Student -> Bool
chromeNeither s = not (chromeXX s) && not (chromeXY s)

noneOfTheAbove :: Student -> Float
noneOfTheAbove = fst . randomR (0.0,100.0) . mkStdGen . (+3) . jeans

developer :: Student -> Float
developer = fst . randomR (0.0,100.0) . mkStdGen . (+4) . jeans

indecisiveness :: Student -> Float
indecisiveness = fst . randomR (0.0,100.0) . mkStdGen . (+5) . jeans

-- totally cool if she or he keeps it him or herself ;)
-- for the purpose of this demo, this is determined by the kind of jeans a student wears. This is not true IRL.
dogmaticBeliefInBinaryBiologicalDeterminism :: Student -> Bool
dogmaticBeliefInBinaryBiologicalDeterminism s = b s && not (chromeNeither s) where
	b = (>99) . fst . randomR ((0,100)::(Int,Int)) . mkStdGen . (+2) . jeans

toZeroOne :: Bool -> Float
toZeroOne x = if x then 1.0 else 0.0

type School = [Student]
type SchoolTreeState = BasicTreeState (School, Student) StdGen
type ActionType = (Student -> Student)

data ActionChangePronoun = ActionChangePronoun Pronoun
instance SmAction ActionChangePronoun SchoolTreeState ActionType where
    action (ActionChangePronoun p) _ = (SUCCESS, (\(Student a _ c d) -> Student a p c d))

data ActionChangeBack = ActionChangeBack
instance SmAction ActionChangeBack SchoolTreeState ActionType where
    action _ _ = (SUCCESS, (\(Student a _ c d) -> Student a a c d))

data ConditionHasProperty = ConditionHasProperty (Student -> Bool)
instance SmCondition ConditionHasProperty SchoolTreeState where
    condition (ConditionHasProperty f) (BasicTreeState (_,s) _) = if f s then SUCCESS else FAIL

-- probably promote this to ConditionRand in Node.hs
data ConditionRandomChance = ConditionRandomChance Float
instance SmCondition ConditionRandomChance SchoolTreeState where
    condition (ConditionRandomChance r) (BasicTreeState _ g) = if rn < r then SUCCESS else FAIL
        where (rn, nextg) = randomR (0.0, 1.0) g

data UtilityProperty = UtilityProperty (Student -> Float)
instance SmUtility UtilityProperty SchoolTreeState where
    utilityOnly (UtilityProperty f) (BasicTreeState (_,s) _) = f s

data UtilityNormalness = UtilityNormalness (Student -> Float)
instance SmUtility UtilityNormalness SchoolTreeState where
    utilityOnly (UtilityNormalness f) (BasicTreeState (s,_) _) = (sum . map f $ s) / (fromIntegral $ length s)

actionChangePronoun :: Pronoun -> SmTreeBuilder SchoolTreeState ActionType ()
actionChangePronoun = addAction . ActionChangePronoun    

actionChangeBack :: SmTreeBuilder SchoolTreeState ActionType ()
actionChangeBack = addAction ActionChangeBack

conditionHasProperty :: (Student -> Bool) -> SmTreeBuilder SchoolTreeState ActionType ()
conditionHasProperty = addCondition . ConditionHasProperty

conditionRandomChance :: Float -> SmTreeBuilder SchoolTreeState ActionType ()
conditionRandomChance = addCondition . ConditionRandomChance

utilityProperty = addUtility . UtilityProperty
utilityNormalness = addUtility . UtilityNormalness


studentTree = utilityWeightedSelector $ do
	sequence $ do
		utilityMultiply $ do
			utilityConst 0.1
			utilityNormalness (toZeroOne . openlyChange)
			utilityProperty feminimity
		actionChangePronoun SheHer
	sequence $ do
		utilityMultiply $ do
			utilityConst 0.1
			utilityNormalness (toZeroOne . openlyChange)
			utilityProperty masculinity
		actionChangePronoun HeHim
	sequence $ do
		utilityMultiply $ do
			utilityConst 0.1
			utilityNormalness (toZeroOne . openlyChange)
			utilityProperty developer
		actionChangePronoun FooBar
	sequence $ do
		utilityMultiply $ do
			utilityConst 0.1
			utilityNormalness (toZeroOne . openlyChange)
			utilityProperty noneOfTheAbove
		actionChangePronoun Other
	sequence $ do
		utilityMultiply $ do
			utilityConst 0.1
			utilityNormalness (toZeroOne . openlyChange)
			utilityAverage $ do
				utilityOneMinus $ do utilityProperty masculinity
				utilityOneMinus $ do utilityProperty feminimity
		actionChangePronoun TheyThem
	sequence $ do
		utilityMultiply $ do
			utilityConst 0.1
			utilityProperty indecisiveness
		actionChangeBack
	sequence $ do
		utilityNormalness ((1-) . toZeroOne . openlyChange)
		-- it's nice to explicitly indicate an intended noop
		-- this line of code is not needed, but all choices should be celebrated
		rSuccess 

makeStudent :: (RandomGen g) => Rand g Student
makeStudent = do
	(isFemale::Bool) <- getRandom 
	(sJeans::Int) <- getRandom
	let 
		pronoun = if isFemale then SheHer else HeHim
	return $ Student pronoun pronoun False sJeans
        
main :: IO ()
main = do
	stdgen <- getStdGen
	students <- replicateM 100 $ evalRandIO makeStudent
	let
		tree = getTree studentTree
		studentfn g s = (g', (foldl (.) id o) s) where
			(rslt, (BasicTreeState _ g'), o) = tickTree tree $ BasicTreeState (students, s) g
		ticktStudents sts = snd $ mapAccumL studentfn stdgen sts
		loop 0 sts = return ()
		loop n sts = do 
			let
				nextsts = ticktStudents sts
			putStrLn . show $ nextsts
			loop (n-1) nextsts
	loop 365 students

-}

main = undefined