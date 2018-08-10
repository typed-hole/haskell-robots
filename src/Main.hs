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
      return ()
    _ -> invalidArgs

invalidArgs :: IO ()
invalidArgs = putStrLn "Invalid arguments. Usage: HaskellRobots.exe <nr. of robots> <direction string>"
