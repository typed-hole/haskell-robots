module Main where

import           Control.Monad.State
import           System.Environment

import           Parsing
import           Robots

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nrRobots, directionString] -> do
      let dirs = evalState parseDirectionString directionString
      let sim = makeSimulation (read nrRobots) dirs
      runSimulation sim
    _ -> invalidArgs

runSimulation :: Simulation -> IO ()
runSimulation sim = do
  putStrLn "Current robot positions:"
  print $ currentRobotPositions sim
  putStrLn "What do we do now?"
  putStrLn "Well we haven't implemented that stuff yet, so nothing ..."

invalidArgs :: IO ()
invalidArgs = putStrLn "Invalid arguments. Usage: HaskellRobots.exe <nr. of robots> <direction string>"
