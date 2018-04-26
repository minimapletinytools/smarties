{-# LANGUAGE TypeSynonymInstances           #-}

module Main where


import Smarties2
import System.Random
import Control.Monad.Random hiding (sequence)
import Prelude hiding (sequence)
import Data.List (mapAccumL, intercalate)

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
feminimity = (/100.0) . fst . randomR (0.0,100.0) . mkStdGen . (+0) . jeans

masculinity :: Student -> Float
masculinity = (/100.0) . fst . randomR (0.0,100.0) . mkStdGen . (+1) . jeans

chromeXX :: Student -> Bool
chromeXX = (<50) . fst . randomR ((0,100)::(Int,Int)) . mkStdGen . (+2) . jeans

chromeXY :: Student -> Bool
chromeXY = (>50) . fst . randomR ((0,100)::(Int,Int)) . mkStdGen . (+2) . jeans

chromeNeither :: Student -> Bool
chromeNeither s = not (chromeXX s) && not (chromeXY s)

noneOfTheAbove :: Student -> Float
noneOfTheAbove = (/100.0) . fst . randomR (0.0,100.0) . mkStdGen . (+3) . jeans

developer :: Student -> Float
developer = (/100.0) . fst . randomR (0.0,100.0) . mkStdGen . (+4) . jeans

indecisiveness :: Student -> Float
indecisiveness = (/100.0) . fst . randomR (0.0,100.0) . mkStdGen . (+5) . jeans

-- totally cool if she or he keeps it him or herself ;)
-- for the purpose of this demo, this is determined by the kind of jeans a student wears. This is not true IRL.
dogmaticBeliefInBinaryBiologicalDeterminism :: Student -> Bool
dogmaticBeliefInBinaryBiologicalDeterminism s = b s && not (chromeNeither s) where
    b = (>99) . fst . randomR ((0,100)::(Int,Int)) . mkStdGen . (+2) . jeans

toZeroOne :: Bool -> Float
toZeroOne x = if x then 1.0 else 0.0

type School = [Student]
type SchoolTreeState = (School, Student)
instance TreeState SchoolTreeState 
type ActionType = (Student -> Student)

actionChangePronoun :: Pronoun -> NodeSequence g SchoolTreeState ActionType ()
actionChangePronoun p = fromAction $ 
    SimpleAction (\_ -> (\(Student a _ _ d) -> Student a p True d))

actionChangeBack :: NodeSequence g SchoolTreeState ActionType ()
actionChangeBack = fromAction $
    SimpleAction (\_ -> (\(Student a _ c d) -> Student a a c d))

conditionHasProperty :: (Student -> Bool) -> NodeSequence g SchoolTreeState ActionType ()
conditionHasProperty f = fromCondition $
    SimpleCondition (\(_, st) -> f st)

utilityProperty :: (Student -> Float) -> NodeSequence g SchoolTreeState ActionType Float
utilityProperty f = fromUtility $
    SimpleUtility (\(_, st) -> f st)

utilityNormalness :: (Student -> Float) -> NodeSequence g SchoolTreeState ActionType Float
utilityNormalness f = fromUtility $
    SimpleUtility (\(sc, _) -> (sum . map f $ sc) / (fromIntegral $ length sc))


studentTree :: (RandomGen g) => NodeSequence g SchoolTreeState ActionType Float
studentTree = utilityWeightedSelector
    [ return . (*0.15) . (+0.01) =<< utilityWeightedSelector 
        [sequence $ do
            a <- utilityNormalness (toZeroOne . openlyChange)
            b <- utilityProperty feminimity
            actionChangePronoun SheHer
            return $ a * b 
        ,sequence $ do
            a <- utilityNormalness (toZeroOne . openlyChange)
            b <- utilityProperty masculinity
            actionChangePronoun HeHim
            return $ a * b 
        ,sequence $ do
            a <- utilityNormalness (toZeroOne . openlyChange)
            b <- utilityProperty developer
            actionChangePronoun FooBar
            return $ a * b 
        ,sequence $ do
            a <- utilityNormalness (toZeroOne . openlyChange)
            b <- utilityProperty noneOfTheAbove
            actionChangePronoun Other
            return $ a * b 
        ,sequence $ do
            a <- utilityNormalness (toZeroOne . openlyChange)
            m <- utilityProperty masculinity
            f <- utilityProperty feminimity
            actionChangePronoun TheyThem
            return $ a * ((1.0-m)+(1.0-f)) / 2.0 
        ]
    ,sequence $ do
        a <- utilityProperty indecisiveness
        actionChangeBack
        return $ 0.01 * a
    ,sequence $ do
        a <- utilityNormalness ((1-) . toZeroOne . openlyChange)
        result SUCCESS 
        return a
        
    ]

makeStudent :: (RandomGen g) => Rand g Student
makeStudent = do
    (sJeans::Int) <- getRandom
    (isFemale::Bool) <- getRandom 
    let 
        pronoun = if isFemale then SheHer else HeHim
    return $ Student pronoun pronoun False sJeans
        
main :: IO ()
main = do
    stdgen <- getStdGen
    students <- replicateM 100 $ evalRandIO makeStudent
    let
        studentfn g s = (g', (foldl (.) id os) s) where
            (g', _, _, os) = runNodeSequence studentTree g (students, s)
        ticktStudents g sts = mapAccumL studentfn g sts
        loop 0 _ sts = return sts
        loop n g sts = do 
            let (g', nextsts) = ticktStudents g sts
            putStrLn . show $ (sum . map (toZeroOne . openlyChange) $ nextsts) --  / (fromIntegral $ length nextsts) 
            loop (n-1) g' nextsts
    sts <- loop 365 stdgen students
    putStrLn $ intercalate "\n" $ map (\s -> show (preferredPronoun s) ++ " " ++ show (assignedPronoun s) ++ " " ++ show (openlyChange s)) sts
