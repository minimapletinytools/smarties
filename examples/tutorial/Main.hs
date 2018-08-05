{-# LANGUAGE TypeSynonymInstances           #-}

module Main where

import Smarties
import System.Random
import System.Console.Haskeline
import System.Exit
import Control.Concurrent
import Control.Monad hiding (sequence)
import Control.Applicative ((<$>))
import Control.Monad.ST
import Control.Lens
import Prelude hiding (sequence)
import Data.List
import Data.List.Index (ifoldl)
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Printf

type Pos = (Int, Int)

addPos :: Pos -> Pos -> Pos
addPos (x1,y1) (x2,y2) = (x1+y1, x2+y2)

data Feelings = Happy | Sad | Hungry | Apathy deriving (Show, Eq)
data Doings = BlowingBubbles | Eating | Bored deriving (Show)

data Slime = Slime {
    _pos :: Pos,
    _feeling :: Feelings,
    _doings :: Doings,
    _weight :: Int
} deriving (Show)

makeLenses ''Slime

-- world size parameters
width :: Int
width = 20
height :: Int
height = 20
numberCells :: Int
numberCells = width * height
neighbors :: [Pos]
neighbors = [(-1,0),(1,0),(0,-1),(0,1)]


wrapCoords :: Pos -> Pos
wrapCoords (x,y) = (x `mod` width, y `mod` height)

-- TODO make sure this does what you want with neg coordinates...
wrapFlattenCoords :: Pos -> Int
wrapFlattenCoords (x,y) = (y `mod` height) * width + x `mod` width

-- behavior tree types
type Slimes = [Slime]
type SlimeGrid = V.Vector (Maybe Slime)
type TreeStateType = (SlimeGrid, Slime)
type ActionType = (Slime -> Slimes)

-- | extract slime that is being operated on from behavior tree perception
getMyself :: NodeSequence g TreeStateType ActionType Slime
getMyself = do
    (_, s) <- getPerception
    return s

-- | extract a neighboring slime
getSlimeRel :: Pos -> NodeSequence g TreeStateType ActionType (Maybe Slime)
getSlimeRel p = do
    (grid, _) <- getPerception
    return $ grid V.! wrapFlattenCoords p

-- | get a list of neigboring slimes
getNeighborSlimes :: NodeSequence g TreeStateType ActionType Slimes
getNeighborSlimes = do
    (grid, s) <- getPerception
    return . mapMaybe ((grid V.!) . wrapFlattenCoords . addPos (_pos s)) $ neighbors

-- behavior tree nodes
-- DELETE
conditionSlimeIsFeeling :: Feelings -> Slime -> NodeSequence g TreeStateType ActionType ()
conditionSlimeIsFeeling f s = fromCondition $
    SimpleCondition (\_ -> _feeling s == f)

actionMoveSlime :: Pos -> NodeSequence g TreeStateType ActionType ()
actionMoveSlime p = fromAction $
    SimpleAction (\_ -> \s -> [set pos (wrapCoords p) s])
-- |
actionMoveSlimeRel :: Pos -> NodeSequence g TreeStateType ActionType ()
actionMoveSlimeRel p = fromAction $
    SimpleAction (\_ -> \s -> [over pos (wrapCoords . addPos p) s])

actionSlime :: NodeSequence g TreeStateType ActionType ()
actionSlime = actionCloneSlimeRel (0,0)

actionCloneSlimeRel :: Pos -> NodeSequence g TreeStateType ActionType ()
actionCloneSlimeRel p = do
    actionMoveSlimeRel p
    actionSlime


-- | our behavior tree
slimeTree :: (RandomGen g) => NodeSequence g TreeStateType ActionType ()
slimeTree = do
    nbs <- getNeighborSlimes
    s <- getMyself
    selector [
        -- no neighbors
        do
            condition (length nbs == 0)
            case _feeling s of
                Happy -> actionMoveSlimeRel (1,0)
                Sad -> actionMoveSlimeRel (-1,0)
                Hungry -> actionMoveSlimeRel (0,1)
                _ -> actionSlime
        -- 1 neighbor
        , do
            condition (length nbs == 1)
            s <- getMyself
            let
                nb = head nbs
            case (_feeling nb, _feeling s) of
                (Happy, Happy) -> actionMoveSlimeRel (0,1)
                (Sad, Sad) -> actionMoveSlimeRel (0,-1)
                (Sad, Happy) -> actionMoveSlime (_pos nb)
                _ -> actionSlime
        ]

        -- > 1 neighbor case
        -- don't do anything, this means the slime will die :(


-- | DELETE
-- our slime action is a special case of behavior tree action type where there should only ever be one action in the output of the tree
-- we do a runtime check here to make sure this is the case
-- unfortunately smarties currently does not support type level checking of this constraint :(.
extractHead :: [ActionType] -> ActionType
extractHead fs
    | null fs = (: [])
    | length fs == 1 = head fs
    | otherwise = error "slime behavior tree must only have one output"

-- | puts slimes in a grid
makeSlimeGrid :: Slimes -> SlimeGrid
makeSlimeGrid slimes = runST $ do
    grid <- MV.replicate numberCells Nothing
    forM_ slimes $ \s@(Slime (x,y) _ _ _) -> MV.write grid (y*width+x) (Just s)
    V.freeze grid

-- | helper for writing slimes to console :)
renderSlime :: Slime -> String
renderSlime (Slime _ f _ _) = case f of
    Happy -> "😊"
    Sad -> "😟"
    Hungry -> "😋"
    Apathy -> "😐"

-- | helper for writing slimes to console :)
renderSlimes :: Slimes -> String
renderSlimes = Data.List.Index.ifoldl func "" . V.toList . makeSlimeGrid where
    func acc i x = output where
        nl = if i+1 `mod` width == 0 then "\n" else ""
        se = case x of
            Just s -> renderSlime s
            Nothing -> " "
        output = printf "%s %s %s" acc se nl

-- | fuse slimes that share the same cell
fuseSlimes :: Slimes -> Slimes
fuseSlimes slimes =  runST $ do
    grid <- MV.replicate numberCells Nothing
    forM_ slimes $ \(Slime (x,y) _ _ w) ->
        MV.modify grid
            (\case
                Just (Slime _ _ _ w2) -> Just $ Slime (x,y) Happy Eating (w+w2)
                Nothing -> Nothing)
            (y*width+x)
    catMaybes . V.toList <$> V.freeze grid

-- | run slimeTree for each slime collecting results
slimeCycle :: (RandomGen g) => g -> Slimes -> (g, Slimes)
slimeCycle g0 slimes = over _2 (fuseSlimes . concat) (mapAccumL runSlimeTree g0 slimes) where
    -- function to run slime tree over all slimes accumulating the RNG
    runSlimeTree g slime = (g', concat (map ($ slime) os)) where
        (g', _, _, os) = execNodeSequence slimeTree g (makeSlimeGrid slimes, slime)

applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)


{-exitLoop :: IO ()
exitLoop = do
    minput <- getInputChar "% "
    case minput of
        Nothing -> return ()
        Just 'q' -> exitSuccess-}

main :: IO ()
main = do
    --forkIO exitLoop
    stdgen <- getStdGen
    let
        genesis = [Slime (0,0) Sad Bored 1] -- 😢
        --outSlimes = applyNtimes 100 (\(g,s) -> slimeCycle g s) (stdgen, genesis)
        cycleOnce n (g,s) = do
            let (g',s') = slimeCycle g s
            putStrLn $ "gen " ++ show n
            putStrLn $ renderSlimes s'
            putStrLn "done"
            threadDelay 100
            cycleOnce (n+1) (g',s')
    cycleOnce 0 (stdgen, genesis)
