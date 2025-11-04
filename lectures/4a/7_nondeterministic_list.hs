import Control.Concurrent
import Control.Monad
import System.IO

type ThreadHandle = MVar ()

populateList :: ThreadHandle -> Int -> Int -> MVar [Int] -> IO ()
populateList handle start end mutableList = do
  return ()

main = do
  return ()
