import Control.Concurrent
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  -- Two threads, each says their name
  forkIO (print "Hello, I am James")
  forkIO (print "Hello, I am Tianyi")

-- ghc -O2 2_no_join_threads.hs -threaded
-- ./2_no_join_threads
