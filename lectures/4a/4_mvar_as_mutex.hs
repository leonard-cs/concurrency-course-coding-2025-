import Control.Concurrent
import Control.Monad
import System.IO

protectedPrint :: String -> MVar () -> MVar ()-> IO ()
protectedPrint s mutex handle = do
  putMVar mutex () -- Acquire the mutex
  print s
  takeMVar mutex -- Release the mutex
  putMVar handle () -- "I am done"

main = do
  hSetBuffering stdout NoBuffering
  -- Two threads, say who they are over multiple print statements,
  -- using protectedPrint
  handle1 <- newEmptyMVar
  handle2 <- newEmptyMVar
  mutex <- newEmptyMVar

  forkIO (protectedPrint "Hello, I am James" mutex handle1)
  forkIO (protectedPrint "Hello, I am Tianyi" mutex handle2)

  takeMVar handle1
  takeMVar handle2

-- ghc -O2 4_mvar_as_mutex.hs -threaded
-- ./4_mvar_as_mutex +RTS -N8