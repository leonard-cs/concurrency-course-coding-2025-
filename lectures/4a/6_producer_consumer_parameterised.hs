import Control.Concurrent
import Control.Monad
import System.Environment
import System.IO

sumChannel :: MVar Int -> Int -> IO Int
sumChannel c 0 = return 0
sumChannel c n = do
  currentValue <- takeMVar c
  remainingValues <- sumChannel c (n - 1)
  return (currentValue + remainingValues)

sendToConsumers :: MVar Int -> Int -> IO ()
sendToConsumers p2c 0 = return ()
sendToConsumers p2c numElems = do
  putMVar p2c 1
  sendToConsumers p2c (numElems - 1)

producer :: MVar Int -> MVar Int -> Int -> Int -> MVar Int -> IO ()
producer p2c c2p numElemsToProduce numConsumers finalResult = do
  sendToConsumers p2c numElemsToProduce
  result <- sumChannel c2p numConsumers
  putMVar finalResult result

consumer :: MVar Int -> MVar Int -> Int -> IO ()
consumer p2c c2p elemsPerConsumer = do
  myResult <- sumChannel p2c elemsPerConsumer
  putMVar c2p myResult

main = do
  args <- getArgs
  let numConsumers = read (args !! 0) :: Int
  let elemsPerConsumer = read (args !! 1) :: Int
  -- Fork a producer
  producerHandle <- newEmptyMVar
  p2c <- newEmptyMVar
  c2p <- newEmptyMVar
  forkIO (producer p2c c2p (numConsumers * elemsPerConsumer) (numConsumers) producerHandle)
  -- Fork numConsumers consumers
  replicateM_ numConsumers (forkIO (consumer p2c c2p elemsPerConsumer))
  -- Join the producer
  result <- takeMVar producerHandle
  -- Print the final result
  print result
  return ()

-- ./6_producer_consumer_parameterised 2 100