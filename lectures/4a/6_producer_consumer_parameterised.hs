import Control.Concurrent
import Control.Monad
import System.Environment
import System.IO

main = do
  args <- getArgs
  let numConsumers = read (args !! 0) :: Int
  let elemsPerConsumer = read (args !! 1) :: Int
  -- Fork a producer
  -- Fork numConsumers consumers
  -- Join the producer
  -- Print the final result
  return ()
