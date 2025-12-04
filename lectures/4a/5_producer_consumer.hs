import Control.Concurrent
import Control.Monad
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

  p2c <- newEmptyMVar :: IO (MVar Int)
  c2p <- newEmptyMVar :: IO (MVar Int)

  -- 1 producer, producing 200 integers,
  --      collecting a result from each consumer
  --      and summing the results.
  producerHandle <- newEmptyMVar :: IO (MVar Int)

  forkIO (producer p2c c2p 200 2 producerHandle)

  -- 2 consumers, each consuming 100 integers and
  --      sending their sum back
  forkIO (consumer p2c c2p 100)
  forkIO (consumer p2c c2p 100)

  result <- takeMVar producerHandle -- get result from consumer

  print (result)

-- ghc -O2 5_producer_consumer.hs -threaded
-- ./5_producer_consumer
