module Robots
    ( Direction(..)
    , makeSimulation
    , step
    ) where

data Direction
    = Up
    | Down
    | Left
    | Right
    deriving (Show, Eq)

newtype Robot
    = MkRobot { getRobot :: (Integer, Integer) }
    deriving (Show, Eq)

newtype Delivery
    = MkDelivery { getLocation :: (Integer, Integer) }
    deriving (Show, Eq)

data SimState
    = SimState { getRobots     :: [Robot]
               , getDeliveries :: [Delivery]
               , getMoves      :: [Direction]
               } deriving (Show, Eq)

newtype Simulation
    = MkSim SimState
    deriving (Show, Eq)

makeSimulation :: Int -> [Direction] -> Simulation
makeSimulation robots moves = let robs = replicate robots (MkRobot (0, 0))
                         in MkSim $ SimState robs [] moves

step :: Simulation -> Simulation
step (MkSim state)
    | null $ getRobots state = MkSim state
    | null $ getMoves state = MkSim state
    | otherwise = let (m:ms) = getMoves state
                      (r:rs) = getRobots state
                   in MkSim state { getMoves = ms
                                  , getRobots = rs ++ [r] }
