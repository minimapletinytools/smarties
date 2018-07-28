## Pronouns Example

In this example, we simulate High School students changing pronouns or whatever 🏳️‍🌈

```haskell
data Pronoun = HeHim | SheHer | TheyThem | FooBar | Other | Undecided deriving (Eq, Show)

data Student = Student {
    assignedPronoun :: Pronoun,
    preferredPronoun :: Pronoun,
    openlyChange :: Bool,
    jeans :: Int
} deriving (Show)

type School = [Student]
type SchoolTreeState = (School, Student)
instance TreeState SchoolTreeState
type ActionType = (Student -> Student)

assignedPronounIs :: Pronoun -> Student -> Bool
assignedPronounIs p s = assignedPronoun s == p

preferredPronounIs :: Pronoun -> Student -> Bool
preferredPronounIs p s = preferredPronoun s == p

feminimity :: Student -> Float
feminimity = (/100.0) . fst . randomR (0.0,100.0) . mkStdGen . (+0) . jeans

masculinity :: Student -> Float
masculinity = (/100.0) . fst . randomR (0.0,100.0) . mkStdGen . (+1) . jeans

noneOfTheAbove :: Student -> Float
noneOfTheAbove = (/100.0) . fst . randomR (0.0,100.0) . mkStdGen . (+3) . jeans

developer :: Student -> Float
developer = (/100.0) . fst . randomR (0.0,100.0) . mkStdGen . (+4) . jeans

indecisiveness :: Student -> Float
indecisiveness = (/100.0) . fst . randomR (0.0,100.0) . mkStdGen . (+5) . jeans

toZeroOne :: Bool -> Float
toZeroOne x = if x then 1.0 else 0.0
```

The first part of the code outlines the state vars that we want operate with/on with our behavior tree plus several helper functions. In particular, we define **SchoolTreeState** which will be the input/computation state (called **perception**) to our behavior tree and **ActionType** which will be its output type.

```haskell
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
```

Next we create several NodeSequences which will be the building blocks for our behavior tree. Smarties contains 4 helpers to facilitate making nodes: **Action**, **Condition**, **Perception** and **Utility**. Each helper has several constructors that represent subsets of a behavior tree operation. You can also use monadic syntax to create NodeSequences. There's a little more boiler plate and the syntax is a little more human readable.

```haskell
studentTree :: (RandomGen g) => NodeSequence g SchoolTreeState ActionType Float
studentTree = utilityWeightedSelector
    [return . (*0.15) . (+0.01) =<< utilityWeightedSelector
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
```

Using our NodeSequences, we define the tree itself. Note that **sequence $ do** is the same as just **do** and used for semantic clarity.

```haskell
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
		studentfn g s = (g', (foldl1 (.) o) s) where
			(rslt, (BasicTreeState _ g'), o) = tickTree tree $ BasicTreeState (students, s) g
		ticktStudents sts = snd $ mapAccumL studentfn stdgen sts
		loop 0 sts = return ()
		loop n sts = do
			let
				nextsts = ticktStudents sts
			putStrLn . show $ nextsts
			loop (n-1) nextsts
	loop 365 students
```

Finally, we run the tree and output the results :D.
