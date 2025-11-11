import Control.Concurrent
import Control.Monad
import System.IO


protectedPrint :: String -> MVar () -> MVar () -> MVar () -> IO ()
protectedPrint s mutex1 mutex2 handle = do
  putMVar mutex1 () -- Acquire the mutex / enter critical section
  putMVar mutex2 ()
  print s
  takeMVar mutex1 -- Release the mutex / leave critical section
  takeMVar mutex2
  putMVar handle () -- "I am done!"


main = do
  hSetBuffering stdout NoBuffering
  -- Two threads, say who they are over multiple print statements,
  -- using protectedPrint
  handle1 <- newEmptyMVar
  handle2 <- newEmptyMVar
  mutex1 <- newEmptyMVar -- shared between our two threads
  mutex2 <- newEmptyMVar -- shared between our two threads
  forkIO (protectedPrint "Hello, I am James" mutex1 mutex2 handle1)
  forkIO (protectedPrint "Hello, I am Tianyi" mutex2 mutex1 handle2)

  takeMVar handle1
  takeMVar handle2

-- ghc -O2 4_mvar_as_mutex.hs -threaded
-- ./4_mvar_as_mutex +RTS -N8
