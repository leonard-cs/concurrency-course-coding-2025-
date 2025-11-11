import Control.Concurrent
import Control.Monad
import System.IO

putChars :: Char -> Int -> IO ()
putChars c 0 = return ()
putChars c n = do
  putChar c
  putChars c (n - 1)


main = do
  hSetBuffering stdout NoBuffering
  -- Launch thread that puts lots of Bs
  forkIO (putChars 'B' 1000)
  -- Put lots of As
  putChars 'A' 1000

-- ghc -O2 1_hello_threads.hs -threaded
-- ./1_hello_threads +RTS -N8
