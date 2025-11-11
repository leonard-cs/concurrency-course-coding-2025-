import Control.Concurrent
import Control.Monad
import System.Environment
import System.IO


data Channel a = Channel (MVar (Stream a)) (MVar (Stream a))

type Stream a = MVar (Item a)

data Item a = Item a (Stream a)

newChannel :: IO (Channel a)
newChannel = do
  emptyStream <- newEmptyMVar
  readEnd <- newMVar emptyStream
  writeEnd <- newMVar emptyStream
  return (Channel readEnd writeEnd)


readChannel :: Channel a -> IO a
readChannel (Channel readEnd _) = do
  readEndStream <- takeMVar readEnd
  (Item value remainder) <- takeMVar readEndStream
  putMVar readEnd remainder
  return value


writeChannel :: Channel a -> a -> IO ()
writeChannel (Channel _ writeEnd) value = do
  newEmptyStream <- newEmptyMVar
  writeEndStream <- takeMVar writeEnd
  putMVar writeEndStream (Item value newEmptyStream)
  putMVar writeEnd newEmptyStream





sumChannel :: Channel Int -> Int -> IO Int
sumChannel c 0 = return 0
sumChannel c n = do
  currentValue <- readChannel c
  remainingValues <- sumChannel c (n - 1)
  return (currentValue + remainingValues)
  

sendToConsumers :: Channel Int -> Int -> IO ()
sendToConsumers p2c 0 = return ()
sendToConsumers p2c numElems = do
  writeChannel p2c 1
  sendToConsumers p2c (numElems - 1)


producer :: Channel Int -> Channel Int -> Int -> Int -> MVar Int -> IO ()
producer p2c c2p numElemsToProduce numConsumers finalResult = do
  sendToConsumers p2c numElemsToProduce
  result <- sumChannel c2p numConsumers
  putMVar finalResult result


consumer :: Channel Int -> Channel Int -> Int -> IO ()
consumer p2c c2p elemsPerConsumer = do
  myResult <- sumChannel p2c elemsPerConsumer
  writeChannel c2p myResult


main = do
  args <- getArgs
  let numConsumers = read (args !! 0) :: Int
  let elemsPerConsumer = read (args !! 1) :: Int
  -- Fork a producer
  producerHandle <- newEmptyMVar
  p2c <- newChannel
  c2p <- newChannel
  forkIO (producer p2c c2p (numConsumers * elemsPerConsumer) numConsumers producerHandle)

  -- Fork numConsumers consumers
  replicateM_ numConsumers (forkIO (consumer p2c c2p elemsPerConsumer))

  -- Join the producer
  result <- takeMVar producerHandle
  -- Print the final result
  print result