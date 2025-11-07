import Control.Concurrent
import Control.Monad
import System.Environment
import System.IO

data Channel = Channel (MVar (Stream a)) (MVar (Stream a))

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
  Item value remainder <- takeMVar readEndStream
  putMVar readEnd remainder
  return value

writeChannel :: Channel a -> a -> IO ()
writeChannel (Channel _ writeEnd) value = do
  newEmptyStream <- newEmptyMVar
  writeEndStream <- takeMVar writeEnd
  putMVar writeEndStream (Item value newEmptyStream)
  putMVar writeEnd newEmptyStream

main
  -- Copy in producer-consumer code, adapt it to use unbounded channels
 = do
  return ()
