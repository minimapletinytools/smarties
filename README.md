# smarties

Smarties is a general purpose [behavior tree](https://en.wikipedia.org/wiki/Behavior_tree_(artificial_intelligence,_robotics_and_control)) library written in Haskell. The library supports utility AI for advanced decision making. Smarties implements many of the design patterns outlined in this [paper](https://course.ccs.neu.edu/cs5150f13/readings/dill_designpatterns.pdf) and some that aren't.

Behavior trees are written in a DSL built with the **NodeSequence** monad. Monadic return values are used for computing utility and passing state between nodes.

## Terminology

- **perception**: input and computation state of the behavior tree. Named perception because it represents how the tree perceives the outside world. **perception** is not mutable when executing the tree and can be used to carry computation state. It is possible to modify the input portion of the **perception**. This is only recommended in the special case where the input state is same as what the tree is operating on as a whole in which case the tree represents a sequential set of operations on a value. e.g. **NodeSequnce g Int (Int->Int)** represents operations on an Int value. In these cases, ensure the **Reduceable p o** constraint is satisfied and use **SelfAction** which is the same as **Action** except also applies the output to the perception.
- **seqence**: control node that executes each child node in sequence until it hits a FAIL node and collects all output.
- **selector**: control node that executes the first SUCCESS node.
- **utility**: optional monadic output for a node that can be used for more complex control flow. For example **utilitySelector** executes the node that has the largest utility.

## Understanding the NodeSequence Monad

**NodeSequence** is a computation that executes all it's internal nodes. At each **>>=** it will check the output and early exit if it reaches a **FAIL**.

**NodeSequence** has the following definition

```haskell
data NodeSequenceT g p o m a =  NodeSequence { runNodes :: g -> p -> (a, g, p, Status, [o]) }
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

## Other

- If you need the MTL style NodeSequence use `Smarties.Trans`, otherwise stick with `Smarties` for better performance

- Please ignore NotSoSmarties. This will be removed in 1.1

## Roadmap: <a id="missing"></a>

- Modelling history patterns is challenging here since the tree produces no side effects. In a previous implementation I could have added a **get/setZipper** methods to a type class constraint on the **perception** type. Currently, as sequences are represented as monads, one could add a monadic if/else that would not be possible to track :(. The current solution is to add something like **markOnExecution :: (Markable p) => String -> NodeSequence g p o ()** and leave the tracking to the user.

- Built in support for [Statistic.Distribution.Normal](https://hackage.haskell.org/package/statistics-0.14.0.2/docs/Statistics-Distribution-Normal.html) for modeling risk reward.
