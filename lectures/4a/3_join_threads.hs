import Control.Concurrent
import Control.Monad
import System.IO


printThenJoin :: String -> MVar () -> IO ()
printThenJoin s handle = do
  print s
  putMVar handle () -- "I am done!"


main = do
  hSetBuffering stdout NoBuffering
  --Two threads, each says their name, then we join them
  handle1 <- newEmptyMVar
  handle2 <- newEmptyMVar

  forkIO (printThenJoin "Hello, I am James" handle1)
  forkIO (printThenJoin "Hello, I am Tianyi" handle2)

  takeMVar handle1
  takeMVar handle2

  return ()

-- ghc -O2 3_join_threads.hs -threaded
-- ./3_join_threads +RTS -N8
