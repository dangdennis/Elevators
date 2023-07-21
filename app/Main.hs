module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.IO
import System.Random

-- Define data types for Direction and Elevator
data Direction = Up | Down | Stay deriving (Show, Eq, Read)

data Elevator = Elevator {elevatorId :: Int, currentFloor :: Int, direction :: Direction} deriving (Show)

data Request = Request {requestFloor :: Int, requestDirection :: Direction} deriving (Show)

-- Simulate movement of elevator
moveElevator :: Elevator -> IO Elevator
moveElevator elevator@(Elevator {currentFloor, direction = dir, elevatorId = id})
  | floor == 1 = return $ Elevator id 2 Up
  | floor == 10 = return $ Elevator id 9 Down
  | dir == Up = return $ Elevator id (floor + 1) Up
  | dir == Down = return $ Elevator id (floor - 1) Down
  | otherwise = return elevator

-- Create and initialize four elevators
initializeElevators :: [Elevator]
initializeElevators = [Elevator i 1 Stay | i <- [1 .. 4]]

-- Simulation loop
simulation :: [Elevator] -> IO ()
simulation elevators = do
  putStrLn "Current status:"
  mapM_ print elevators
  putStrLn "-----------------------"
  threadDelay 1000000
  newElevators <- mapM moveElevator elevators
  simulation newElevators

main :: IO ()
main = do
  putStrLn "Starting simulation..."
  simulation initializeElevators
