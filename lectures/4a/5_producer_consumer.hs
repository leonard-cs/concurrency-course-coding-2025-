import Control.Concurrent
import Control.Monad
import System.IO

sendToConsumers :: MVar Int -> Int -> IO ()
sendToConsumers p2c numElems = return ()

getFromConsumers :: MVar Int -> Int -> IO Int
getFromConsumers c2p numConsumers = return 0

producer :: MVar Int -> MVar Int -> Int -> Int -> MVar Int -> IO ()
producer p2c c2p numElemsToProduce numConsumers finalResult = return ()

consumeData :: MVar Int -> Int -> IO Int
consumeData p2c elems = return 0

consumer :: MVar Int -> MVar Int -> Int -> IO ()
consumer p2c c2p elemsPerConsumer = return ()

main = do

  -- 1 producer, producing 200 integers, collecting a result from each producer
  -- and summing the results.
  --
  -- 2 consumers, each consuming 100 integers and sending their sum back

  return ()
