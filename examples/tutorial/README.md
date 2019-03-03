run `stack exec tutorial`

```
🌱🌱🌱🌱🌱😱🌱🌱🌱😱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱😱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱😱😱🌱😱🌱🌱🌱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱😱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱😱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱😱🌱🌱🌱🌱😱😱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱😱😱🌱🌱🌱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱😱😱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱😱😱🌱🌱🌱😱😱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱😱🌱😱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱😱😱😱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱😱😱😱🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱😱🌱🌱🌱🌱🌱🌱🌱😱😱
🌱🌱🌱🌱🌱🌱🌱😱🌱🌱🌱😱🌱🌱🌱🌱🌱🌱😱😱
🌱🌱🌱🌱🌱🌱😱😱🌱🌱🌱😱🌱🌱🌱🌱🌱🌱😱😱
🌱🌱🌱🌱🌱😱🌱🌱🌱🌱🌱😱🌱🌱🌱🌱🌱🌱🌱🌱
🌱🌱🌱🌱🌱🌱🌱🌱🌱🌱😱🌱🌱🌱🌱🌱🌱🌱🌱🌱
```

## Tutorial

In this example, we'll use smarties to implement the rules for [Conway's Game of Life (CGoL)](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).

First we declare some constants and a bunch of helper methods to form grid which we'll be running our CGoL simulation on.

```haskell
-- world size parameters
width :: Int
width = 20
height :: Int
height = 20
numberCells :: Int
numberCells = width * height
```

We'll represent the CGoL world as a `Vector Bool` and for this, we'll need a bunch of indexing hepler functions.

```haskell
-- types
type Pos = (Int, Int)

addPos :: Pos -> Pos -> Pos
addPos (x1,y1) (x2,y2) = (x1+x2, y1+y2)

wrapCoords :: Pos -> Pos
wrapCoords (x,y) = (x `mod` width, y `mod` height)

flattenCoords :: Pos -> Int
flattenCoords (x,y) = y * width + x

wrapFlattenCoords :: Pos -> Int
wrapFlattenCoords = flattenCoords . wrapCoords

indexToPos :: Int -> Pos
indexToPos p = (p `mod` width, p `div` width)
```

Finally, we'll add two helper methods that we'll use later on to count the number of live neighbors a grid cell has.

```haskell

neighbors :: [Pos]
neighbors = [(-1,0),(1,0),(0,-1),(0,1),(1,1),(1,-1),(-1,1),(-1,-1)]

countTrue :: [Bool] -> Int
countTrue = foldl (\acc x -> if x then acc+1 else acc) 0
```

Next we declare smarties types used in the smarties node monad `NodeSequence g p o a`. `p` is `TreeStateType` and `o` is `ActionType`. `a` is the monadic return type and `g` is any instance of `RandomGen`. `g` is `StdGen` in this example and it doesn't matter since there is no randomness in CGoL.

```haskell
type Grid = V.Vector Bool
type TreeStateType = (Pos, Grid)
type ActionType = Bool
```

Next we use a variety of helpers and monads syntax to declare nodes we need to implement the rules of CGoL.

```haskell
countNeighbors  :: NodeSequence g TreeStateType ActionType Int
countNeighbors = do
  (pos, grid) <- getPerception
  return . countTrue . map ((grid V.!) . wrapFlattenCoords . addPos pos) $ neighbors

ifNeighborsMoreThan :: Int -> NodeSequence g TreeStateType ActionType ()
ifNeighborsMoreThan x = do
  n <- countNeighbors
  condition (n > x)

ifNeighborsLessThan :: Int -> NodeSequence g TreeStateType ActionType ()
ifNeighborsLessThan = flipResult . ifNeighborsMoreThan . (\x -> x-1)

die :: NodeSequence g TreeStateType ActionType ()
die = fromAction $ SimpleAction (\_ -> False)

born :: NodeSequence g TreeStateType ActionType ()
born = fromAction $ SimpleAction (\_ -> True)

ifAlive :: NodeSequence g TreeStateType ActionType ()
ifAlive = do
  (pos, grid) <- getPerception
  condition $ grid V.! (wrapFlattenCoords pos)

ifDead :: NodeSequence g TreeStateType ActionType ()
ifDead = flipResult $ ifAlive
```

Now we have all the tools we needt o write our behavior tree.

```haskell
conwayTree :: NodeSequence g TreeStateType ActionType ()
conwayTree = do
  selector [
    -- rule 1. Any live cell with fewer than two live neighbors dies, as if by underpopulation.
    do
      ifAlive
      ifNeighborsLessThan 2
      die
    -- rule 3. Any live cell with more than three live neighbors dies, as if by overpopulation.
    , do
      ifAlive
      ifNeighborsMoreThan 3
      die
    -- rule 4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
    , do
      ifDead
      ifNeighborsMoreThan 2
      ifNeighborsLessThan 4
      born]
```

Next we need to write some code to actually run the tree using `execNodeSequence`. We run the tree once for each cell in the grid. Doing diligence here, we `mapAccumL` the random generator `g` through each execution. For CGoL this is unnecessary as the generator is never used.

Smarties allows behavior trees to produce several outputs that are appended to a list. Thus `execNodeSequence` will give us a `[Bool]` representing the output. In our example, the tree will produce `True` or `False` if cell should live or die respectively. There should never be more than one element in the list but we still use the last element. If the list is empty then there should be no in the cell. Thus we convert the output `[Bool]` into a `Bool -> Bool` that we can `zipWith ($)` with our grid to produce the next grid for the next generation.

```haskell
runConway :: g -> Grid -> (g, Grid)
runConway g0 grid = (g', V.zipWith ($) (V.fromList fns) grid) where
  (g', fns) = mapAccumL runTree g0 (map indexToPos [0..numberCells])
  runTree g pos = (g'', if null os then id else \_ -> last os) where
    (g'', _, _, os) = execNodeSequence conwayTree g (pos, grid)
```

One last function to render our simulation.

```haskell
-- | helper for writing grid to console :)
renderGrid :: Grid -> String
renderGrid = Data.List.Index.ifoldl func "" . V.toList where
    func acc i x = output where
        nl = if (i+1) `mod` width == 0 then "\n" else ""
        se = case x of
            True -> "😱"
            False -> "🌱"
        output = printf "%s%s%s" acc se nl
```

And finally, our main function that repeatedly runs the simulation and updates the state.

```haskell
-- | go!
main :: IO ()
main = do
    stdgen <- getStdGen
    let
        genesis = V.fromList . take numberCells $ randoms stdgen
        cycleOnce (n :: Int) (g, s) = do
            putStrLn "done"
            printf "gen %d\n" n
            putStrLn $ renderGrid s
            let (g',s') = runConway g s
            threadDelay 100000
            cycleOnce (n+1) (g',s')
    cycleOnce 0 (stdgen, genesis)
```
