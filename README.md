# smarties

Smarties is a general purpose [behavior tree](https://en.wikipedia.org/wiki/Behavior_tree_(artificial_intelligence,_robotics_and_control)) library written in Haskell. Smarties supports utility computation functions within the trees for advanced decision making. Smarties implements many of the design patterns outlined in this [paper](https://course.ccs.neu.edu/cs5150f13/readings/dill_designpatterns.pdf) and probably some that aren't.

Smarties comes in 2 parts. The core library containing all the tree logic and a special State Monad that enables a DSL for writing compile-time behavior trees in an intuitive way. The core library provides basic control and utility nodes and the user implements their own *condition*, *action*, *utility* and *perception* nodes that operates on their own data type.

_This library is still in alpha and actively being developed. The next version will be on hackage and will include several major API changes that should greatly improve useability. Please see Smarties2 in source for a sneak preview and read [Future Development](#missing) for more details_

## Example
```haskell
data Pronoun = HeHim | SheHer | TheyThem | FooBar | Other | Undecided deriving (Eq)

data Student = Student {
    assignedPronoun :: Pronoun,
    preferredPronoun :: Pronoun,
    openlyChange :: Bool,
    jeans :: Int
}

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
```

The first part of the code outlines the state vars that we want operate with/on with our behavior tree plus several helper functions.

This next part creates some type synonyms that we'll pass in as type vars in our node declaration.

```haskell
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
```

The core node type is *SmNode*. Smarties provides *SmCondition*, *SmAction*, *SmUtility* and *SmPerception* convenience wrappers around it. For the most part, you will not be using SmNode directly. The part of the code uses these wrappers to create several nodes that we'll use to compose our logic. 

```haskell
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
```

Here are some convenience wrappers to improve the syntax of the tree. These convert *SmNode* types to their respective *SmTreeBuilder* computations. These won't be necessary in v2.0 of the library.

```haskell
tree = utilityWeightedSelector $ do
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
```

Next is the tree itself written using in the *TreeBuilder* State monad. Note that this monad only _creates_ the tree. It does not run the tree. 

The monad follows conventional behavior tree semantics. Branches are created using "do" syntax so you can read the tree hierarchy from the indentation.

Utility is handled a little bit strangely here. There are other good choices and I like the one I chose here:

UtilitySelector and WeightedUtilitySelector are the only two nodes for making meaningful decisions based on utility. UtilitySelector computes utility of all its child nodes and picks the one with max where as WeightedUtilitySelector makes a random choice.

*Utility* is expected to be normalized in the range of (0,1). Consequently, the library provides no functions like UtilityAdd or UtilitySubstract. This is not strictly enforced and you are free to break this rule. In fact, while the library provides only nodes that follow this rule, it does it through data types that takes operations to perform on the utilities of the nodes children. It is very easy to write your own utility math nodes that break this rule so do so with caution! 

*Utility* has two additional values, *U_FAIL* and *U_PASS*. These model the opt-out patterns. The two utility selectors will never run a node that has utility *U_FAIL*. For the Opt-In pattern, simply return a utility of 1 and UtilitySelector will pick the earlier child in case of a tie. Generally speaking, by combining various control nodes, it's possible to model many different utility based decision making patterns. 

*Sequence* has utility of its first child with a Utility value assuming all children preceeding it have utility *U_PASS*. This way, it's possible to provide several opt-out scenarios before even considering executing the actions at the end of a sequence.

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

## Advanced features

If you look at the methods in *TreeState* you will see it contains many stack and indexing operations. *AdvancedTreePerception x y g* provides a default implemantion of these using *TreeStack x*. The purpose of this is to allow nodes to create state for future nodes to operate on. For example, lets say each student in our example has a best friend and a crush. These students _their_ friends/crushes strongly strongly influence the student's decision to change pronouns. Also apologies for very subtly suggesting that monogamy is the only option... Naively approaching this, we may find ourselves creating several nodes:

```haskell
data ConditionBestFriendFoo ... 
data ConditionWorstFriendFoo ...
data ConditionBestFriendOfBestFriendFoo ...
data ConditionBestFriendOfWorstFriendFoo ...
```

instead, we can mark students in our tree's computation state allowing us to structure our nodes as such:

```haskell
data PerceptionMarkSelf ...
data PerceptionMarkBestFriendOfMarkedStudent ...
data PerceptionMarkCrushOfMarkedStudent ...
data ConditionMarkedStudentFoo ...
```

allowing nodes to change the computation context of future nodes. This is both more general and signficantly reduces the amount of needed nodes for creating more complex computation spaces.

The type variable *x* in *AdvancedTreePerception* is used to pass this information around the tree. The reason it's embedded in *TreeStack x* is to add scoping behavior to this information. Apologies for this terrible example and I hope you can imagine better use cases for this feature:

```haskell
sequence $ do
	markBestFriend
	conditionMarkedStudentFoo
	scope $ do
		markWorstFriend
		conditionMarkedStudentFoo
	-- not needed!
	-- markBestFriend 
	doSomethingToMarkStudent
```

*AdvancedTreePerception* is general sufficient enough for all needs I can think of. The only reason things are done through the *TreeState* typeclass is precisely so the library can _hide_ some of the generality of *AdvancedTreePerception*. Of course, you are free to implement your own *TreeState* and you _absolutely should_ if you can come up with a better interface than *AdvancedTreePerception* D:. 

*Utility* values are actually stored as distributions. For the most part, you will only need the mean part of the distributions but the variance is useful for calculating things like risk-reward. The most common use case of this is modelled in *UtilityCharacteristic Float* which creates new distribution by composing its child's distribution with the characteristic function on [x,1]. Semantically speaking, the utility is a random variable representing our outcome. *UtilityCharacteristic x* says if we have utility less than *x* than we have utility of *0*. 


## Future Development: <a id="missing"></a>

- v2.0 will incorporate some of the ideas below to improve the library's interface and type safety.

- Modelling history patterns is challenging here since the tree produces no side effects. It's easy to add another parameter to update containing a zipper tracking which node we are at and nodes can manage their records in the state. Alternatively, the builder can flag each node with a unique index for the same purpose. A full blown solution may include default parameters in these records such as number of times run, and last success. The downside here is further expanding the already bloated TreeState typeclass.

- The builder and some Nodes currently resort to run-time errors on invalid states that could have been determined at compile time. In partiuclar decorator nodes assert in the builder if they have more than one children and utility branch nodes assert if they have children that don't have utility. The former problem can be solved in *SmTreeBuilder* by using the return type to keep track of the number of children. This is rather silly IMO and complicates the DSL by requiring nodes to be >>= chained together. Still, this is Haskell and if there isn't type level safety than why is it in Haskell? The yet to be implemented *SafeSmTreeBuilder* will do just this. My solution to the latter problem *SmNode* instances to carry yet another type variable to distinguish the node type. I'me shelving this expansion for now as my experience in Haskell is limited and I suspect an entirely different approach (such as the one outlined just below here) would be more appropriate if I were to solve this problem. 

- The tree itself is not unlike the State monad as it represents a computation with state and output. It should be possible to make a monad instance out of SmNode. A more interesting thought is wether SmTreeBuilder is even necessary i.e. if SmNode were a Monad, then the DSL could construct SmNode directly. I drafted this idea out in my head and the major obstacle is that the multi child branch nodes such as sequence and selector run different computations based on the output of its children. With a little trickery in the monad implemenation I think this is possible. With this, nodes can be used directly in the monad syntax. TreeBuilder currently uses a handful of helper functions mapping the node constructors to their respective builder monads. The monad can also wrap *RandT* making *RandomGen* optional without the noop *RandomGen (TreeState p)* instance.

- Many kind redditors of _/r/haskell strongly suggested taking [this](https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/) approach instead which is appropriate here. I'll have to draft out an implementation to see if this approach makes more sense. It's probably the better way to go since I'm not leveraging the type safety that the type class implementation might afford me.

- It's become clear to me that rather than *UtilityFunc1* and *UtilityFuncN*, it's better to have something like *SelectorWithUtilityFunc* and *SequenceWithUtilityFunc*. This way you can easily distribute math functions accross sequences/selectors and not have to copy paste them like I did in the example. Still another option I'm exploring is using the output value of the (to be implemented) tree monad for utility which both adds type level safety to nodes that have/do not have utility and allows users to intuitively make their own utility functions using regular haskell. e.g.
```haskell
sequence $ do
	x <- someUtility
	y <- someUtility
	return (x * y)
```

- I did consider having a *Distribution* typeclass used by Utility. I did not come up with a good interface for this and the basic gaussian distribution is a reasonable limitation.

