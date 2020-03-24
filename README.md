[![Build Status](https://travis-ci.com/pdlla/smarties.svg?branch=master)](https://travis-ci.com/pdlla/smarties)

# smarties
Smarties is a general purpose [behavior tree](https://en.wikipedia.org/wiki/Behavior_tree_(artificial_intelligence,_robotics_and_control)) library written in Haskell. The library supports utility AI for advanced decision making. Smarties implements many of the design patterns outlined in this [paper](https://course.ccs.neu.edu/cs5150f13/readings/dill_designpatterns.pdf) and some that aren't.

Behavior trees are written in a DSL built with the **NodeSequence** monad. Monadic return values are used for computing utility and passing state between nodes.

To jump right in, please see the this tutorial example implementing [Conway's Game of Life](https://github.com/pdlla/smarties/tree/master/examples/tutorial). There are other examples in the examples folder that I either put in too little or too much effort.

## Terminology
- **perception**: input and computation state of the behavior tree. Named perception because it represents how the tree perceives the outside world. In the current implementation it's possible to write nodes that modify **perception** but this is not recommended and this feature may be removed in a future release.
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

**NodeSequence** looks a lot like **StateT (p,g) Writer [o]** except with an additional Status output. The difference is that with each **>>=** if the input computation has Status **FAIL**, the monad will stop passing on **p** and appending to **[o]**. Note that it will continue to pass through **g** and evaluate the monadic return value **a**. Thus running **NodeSequence** produces an **a** and two thunks representing the collected state and output up until the first **FAIL**.

The monadic return value is useful for passing general information between nodes. For example it's possible to implement loops:

```haskell
howQueerIsMyFriend = sequence $ do
	x <- getFriend
	n <- numberFriendsOf x
	clique <- forM [0..(n-1)] (\n' -> do
		s <- getFriendOf x n'
		return queerness s
		)
	return (mean clique)
```

## Other
- If you need the MTL style NodeSequence use `Smarties.Trans`, otherwise stick with `Smarties` for better performance

- Smarties gives access to the (rather simple) behavior tree control methods in `Smarties.Nodes`. Most of its power comes from the flexibility of monadic syntax. In some cases, it may be better to use something like **StateT (p,g) Writer [o]**. Sequence and selectors are still possible with monadic operations like [`ifM`](https://hackage.haskell.org/package/extra-1.7.1/docs/Control-Monad-Extra.html).

## Additional Features: <a id="missing"></a>
Some ideas for features to add to this package. I'll probably never get to these but feel free to submit a PR.

- Built in support for [Statistic.Distribution.Normal](https://hackage.haskell.org/package/statistics-0.14.0.2/docs/Statistics-Distribution-Normal.html) for modeling risk reward. This includes [basic](https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables) [operations](https://ccrma.stanford.edu/~jos/sasp/Product_Two_Gaussian_PDFs.html) on distributions.

- It is possible to modify **perception** during tree execution. This is only recommended in the special case where the input state is same as what the tree is operating on as a whole in which case the tree represents a sequential set of operations on a value. e.g. **NodeSequnce g Int (Int->Int)** represents operations on an Int value. In these cases, ensure the **Reduceable p o** constraint is satisfied and use **SelfAction** which is the same as **Action** except also applies the output to the perception. The current implementation is a little too idiosynchratic and not well documented. This feature may be removed all together. In this case **perception** will be changed to immutable, i.e. p removed from the RHS of `runNodes`
```
data NodeSequenceT g p o m a =  NodeSequence { runNodes :: g -> p -> (a, g, Status, [o]) }
```
This is a working feature but I mention it in this section to indicate it is subject to change.
