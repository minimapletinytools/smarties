[![Build Status](https://travis-ci.com/pdlla/smarties.svg?branch=master)](https://travis-ci.com/pdlla/smarties)

# smarties
Smarties is a general purpose [behavior tree](https://en.wikipedia.org/wiki/Behavior_tree_(artificial_intelligence,_robotics_and_control)) (BT) library written in Haskell. The library supports utility AI for advanced decision making. Smarties implements many of the design patterns outlined in this [paper](https://course.ccs.neu.edu/cs5150f13/readings/dill_designpatterns.pdf) and some that aren't.

BTs are written in a DSL built with the **NodeSequence** monad. Monadic return values are used for computing utility and passing state between nodes.

To jump right in, please see the this tutorial example implementing [Conway's Game of Life](https://github.com/pdlla/smarties/tree/master/examples/tutorial). There are other examples in the examples folder that I either put in too little or too much effort.

## Terminology
- **perception**: input and computation state of the BT. Named perception because it represents how the tree perceives the outside world. It's possible to write nodes that modify **perception** so that your BT has mutable perception (or state). Since you are already writing in Haskell, you probably don't ever want to do this.
- **sequence**: control node that executes each child node in sequence until it hits a FAIL node and collects all output.
- **selector**: control node that executes the first SUCCESS node.
- **utility**: optional monadic output for a node that can be used for more complex control flow. For example **utilitySelector** executes the node that has the largest utility.

## Understanding the NodeSequence Monad
**NodeSequence** is a computation that executes all it's internal nodes. At each **>>=** it will check the output and early exit if it reaches a **FAIL**.

**NodeSequence** has the following definition

```haskell
data NodeSequenceT g p o m a =  NodeSequence { runNodeSequenceT :: g -> p -> (a, g, p, Status, [o]) }
```

The sequence represents a computation that takes a generator and perception and returns an output with the following types:

- **a**: monad output type, typically used for computing utility
- **g**: random generator
- **p**: perception type
- **Status**: Status of executing NodeSequence, either **SUCCESS** or **FAIL**
- **o**: output type (or action type)

**NodeSequence** looks a lot like **StateT (p,g) Writer [o]** except with an additional Status output. The difference is that with each **>>=** if the input computation has Status **FAIL**, the monad will stop accumulating changes on **p** and appending to **[o]**. Note that it will continue to pass through **p** and **g** to evaluate the monadic return value **a** which is needed for things like utility selectors. Thus running **NodeSequence** produces an **a** and two thunks representing the perception and output up until the first **FAIL**.

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

## Builders
Smarties provides the `Smarties.Builders` module for building your own logic nodes which are needed to actually use Smarties in a project. It supports the following types of nodes:

- `Condition`: create a condition node
- `Action`: create an action node
- `Utility`: create a node that returns a utility score
- `Perception`: create a node that modify the perception

Each builder (except for `Perception`) has a simple variant (prefixed by `Simple`) which ensures the **perception** is immutable. You'll want to use the simple variants in most cases.

To keep the syntax simple in most cases, there are non-transformer variants of each builder which wrap the transformer ones.

## Other
- Smarties gives access to the (rather simple) BT control methods in `Smarties.Nodes`. Most of its power comes from the flexibility of monadic syntax. In some cases, it may be better/simpler to use something like **StateT (p,g) Writer [o]**. Sequence and selectors are still possible with monadic operations like [`ifM`](https://hackage.haskell.org/package/extra-1.7.1/docs/Control-Monad-Extra.html).

## Additional Features: <a id="missing"></a>
Some ideas for features to add to this package. I'll probably never get to these but feel free to submit a PR.

- Built in support for [Statistic.Distribution.Normal](https://hackage.haskell.org/package/statistics-0.14.0.2/docs/Statistics-Distribution-Normal.html) for modeling risk reward. This includes [basic](https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables) [operations](https://ccrma.stanford.edu/~jos/sasp/Product_Two_Gaussian_PDFs.html) on distributions.

- It is possible to modify **perception** during tree execution. This is only recommended in the special case where the input state is same as what the tree is operating on as a whole in which case the tree represents a sequential set of operations on a value. e.g. **NodeSequence g Int (Int->Int)** represents operations on an Int value. In these cases, ensure the **SelfActionable p o** constraint is satisfied and use **SelfAction** which is the same as **Action** except also applies the output to the perception. The current implementation is a little idiosyncratic and I may remove in the future so it's mentioned here for now.
