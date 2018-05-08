# smarties

Smarties is a general purpose [behavior tree](https://en.wikipedia.org/wiki/Behavior_tree_(artificial_intelligence,_robotics_and_control)) library written in Haskell. The library supports utility AI for advanced decision making. Smarties implements many of the design patterns outlined in this [paper](https://course.ccs.neu.edu/cs5150f13/readings/dill_designpatterns.pdf) and some that aren't.

Behavior trees are written in a DSL built with the **NodeSequence** monad. Monadic return values are used for computing utility and passing state between nodes.

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

## Terminology

- **perception**: input and computation state of the behavior tree. Named perception because it represents how the tree perceives the outside world. **perception** is not mutable when executing the tree and can be used to carry computation state. It is possible to modify the input portion of the **perception**. This is only recommended in the special case where the input state is same as what the tree is operating on as a whole in which case the tree represents a sequential set of operations on a value. e.g. **NodeSequnce g Int (Int->Int)** represents operations on an Int value. In these cases, ensure the **Reduceable p o** constraint is satisfied and use **SelfAction** which is the same as **Action** except also applies the output to the perception.
- **seqence**: control node that executes each child node in sequence until it hits a FAIL node and collects all output.
- **selector**: control node that executes the first SUCCESS node.
- **utility**: optional monadic output for a node that can be used for more complex control flow. For example **utilitySelector** executes the node that has the largest utility.

## Understanding NodeSequence

**NodeSequence** is a computation that executes all it's internal nodes. At each **>>=** it will check the output and early exit if it reaches a **FAIL**. **sequence** is exactly the same as **NodeSequence** except that it will create a scope on the perception.

**NodeSequence** has the following definition

```haskell
data NodeSequence g p o a =  NodeSequence { runNodes :: g -> p -> (a, g, p, Status, [o]) }
```

The sequence represents a computation that takes a generator and perception and returns an output with the following types:

- **a**: monad output type, typically used for computing utility
- **g**: random generator
- **p**: perception type
- **Status**: Status of executing NodeSequence, either **SUCCESS** or **FAIL**
- **o**: output type

**NodeSequence** looks a lot like **Statet (p,g) Writer [o]** except with an addition Status output. The difference lies that with each **>>=** if the input computation has Status FAIL, the monad will stop passing on **p** and appending to **[o]**. It will continue to pass through **g** and evaluate **a**. Thus running **NodeSequence** produces an **a** and two thunks representing the collected state and output of the represented sequence up until the first **FAIL**. These thunks may or may not be evaluated depending on the decisions made by the parent nodes.

In the example above, the monadic return values are used only for computing utility. This value is useful for passing general information between nodes. Another common usage pattern is for implementing loops. For example:

```haskell
crushCliqueFemininity = sequence $ do
	x <- findCrush
	n <- numberStudentsAround x
	clique <- forM [0..(n-1)] (\n' -> do
		s <- getStudentAround x n'
		return feminimity s
		)
	return (mean clique)
```

## Roadmap: <a id="missing"></a>

- Modelling history patterns is challenging here since the tree produces no side effects. In a previous implementation I could have added a **get/setZipper** method to **TreeState** tracking which node we are at and nodes can manage their records in the state. Currently, as sequences are represented as monads, one could add a monadic if/else that would not be possible to track :(. The current solution is to add **markOnExecution :: (Markable p) => String -> NodeSequence g p o ()** and leave the tracking to the user.

- I'm considering removing the **TreeState** type class constraint all together and making the tree entirely stateless. Instead, monadic return values should be the only method for passing information between nodes.

- Support for [Statistic.Distribution.Normal](https://hackage.haskell.org/package/statistics-0.14.0.2/docs/Statistics-Distribution-Normal.html) for modelling risk reward.

- The next version will probably promote **NodeSequence** to **NodeSequenceT**. This will be useful if, for example, there is state info stored in a mutable type. ~~Also considering using the **RandT** monad instead of reimplementing it in **NodeSequence** as it is right now.~~
