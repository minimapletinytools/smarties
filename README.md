# smarties

Smarties is a general purpose [behavior tree](https://en.wikipedia.org/wiki/Behavior_tree_(artificial_intelligence,_robotics_and_control)) library written in Haskell. Smarties supports utility AI for advanced decision making. Smarties implements many of the design patterns outlined in this [paper](https://course.ccs.neu.edu/cs5150f13/readings/dill_designpatterns.pdf) and probably some that aren't.

Behavior trees are written in a DSL built with the *NodeSequence Monad* and return values are used for computing utility.

## Example
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
```

The first part of the code outlines the state vars that we want operate with/on with our behavior tree plus several helper functions. In particular, we define *SchoolTreeState* which will be the input/computation state (called *perception*) to our behavior tree and *ActionType* which will be its output type.

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

Next we create several NodeSequences which will be the building blocks for our behavior tree. 

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

Using our NodeSequences, we define the tree itself. Note that there is overlap between *NodeSequence* and *sequence*. 

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

## Terminology

- *seqence*:
- *selector*:
- *utility*:
- *perception*:

## Understanding NodeSequence

*NodeSequence* is a computation that executes all it's internal nodes. At each *>>=* it will check the output and early exit if it reaches a *FAIL*. *sequence* is exactly the same as *NodeSequence* except that it will create a scope on the perception.

*NodeSequence* has the following definition

```haskell
data NodeSequence g p o a =  NodeSequence { runNodes :: g -> p -> (a, g, p, Status, [o]) }
```

The sequence represents a computation that takes a generator and perception and returns an output with the following types:

- *a*: monad output type, typically used for computing utility
- *g*: random generator
- *p*: perception type
- *Status*: Status of executing NodeSequence, either *SUCCESS* or *FAIL*
- *o*: output type

*NodeSequence* looks a lot like *Statet (p,g) Writer [o]* except with an addition Status output. The difference lies that with each *>>=* if the input computation has Status FAIL, the monad will stop passing on *p* and appending to *[o]*. It will continue to pass through *g* and evaluate *a*. Thus running *NodeSequence* produces an *a* and two thunks representing the collected state and output of the represented sequence up until the first *FAIL*. These thunks may or may not be evaluated depending on the decisions made by the parent nodes. 

## Advanced features



If you look at the methods in *TreeState* you will see it contains many stack operations. *AdvancedTreePerception x y* provides a default implemantion of these using *TreeStack x*. The purpose of this is to allow nodes to create state for future nodes to operate on. For example, lets say each student in our example has a best friend and a crush. These students _their_ friends/crushes strongly strongly influence the student's decision to change pronouns. Also apologies for very subtly suggesting that monogamy is the only option... Naively approaching this, we may find ourselves creating several nodes:

```haskell
conditionBestFriendFoo = ... 
conditionWorstFriendFoo = ...
conditionBestFriendOfBestFriendFoo = ...
conditionBestFriendOfWorstFriendFoo = ...
```

instead, we can mark students in our tree's computation state allowing us to structure our nodes as such:

```haskell
markSelf = ...
markBestFriendOfMarkedStudent = ...
markCrushOfMarkedStudent = ...
conditionMarkedStudentFoo = ...
```

allowing nodes to change the computation context of future nodes. This is both more general and signficantly reduces the amount of needed nodes for creating more complex computation spaces.

The type variable *x* in *AdvancedTreePerception* is used to pass this information around the tree. The reason it's embedded in *TreeStack x* is to add scoping behavior to this information. For example

```haskell
-- TODO
```

*AdvancedTreePerception* is sufficient enough for all needs I can think of. The only reason things are done through the *TreeState* typeclass is precisely so the library can _hide_ some of the generality of *AdvancedTreePerception*. 

## Future Development: <a id="missing"></a>

- The *TreeState* typeclass has support for a looping index but there are no supporting control nodes. A looping index makes our language turing complete :). Besides good feels though, the main use of this is for code reuse. For example:

```haskell
	selectorForEach $ do
		selectNthSquare
		i <- getSelection
		guard $ i `mod` 2 == 0
		colorSelectedSquare
```

- Modelling history patterns is challenging here since the tree produces no side effects. It's easy to add another parameter to update containing a zipper tracking which node we are at and nodes can manage their records in the state. Alternatively, the builder can flag each node with a unique index for the same purpose. A full blown solution may include default parameters in these records such as number of times run, and last success. The downside here is further expanding the already bloated TreeState typeclass.

- Currently it impossible to capture perception changes in child nodes due to scoping in sequences and selectors. This is desired behavior until it's not. For example the following use case is not possible:
```haskell
	sequence $ do
		selector 
			[sequence $ do
				conditionA
				changePerceptionA
			,sequence $ do
				conditionB
				changePerceptionB
			]
		-- the perception will not contain any info from the previously run selector node
		... 
```
The next version will contain *selectorNoScope* which omits scoping behavior. I'm not convinced *sequenceNoScope* is necessary and implementing it requires adding another parameter to the underlying state transformations.

- Support for [Statistic.Distribution.Normal](https://hackage.haskell.org/package/statistics-0.14.0.2/docs/Statistics-Distribution-Normal.html). A distribution a useful model for risk/reward based decision making.

