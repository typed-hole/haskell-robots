module Robots
    ( Direction(..)
    , simulate
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

simulate :: Int -> [Direction] -> Simulation
simulate robots moves = let robs = replicate robots (MkRobot (0, 0))
                         in MkSim $ SimState robs [] moves
