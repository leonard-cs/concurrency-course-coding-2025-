import Control.Concurrent
import Control.Monad
import System.IO

--putChars :: Char -> Int -> IO ()
main = do
  hSetBuffering stdout NoBuffering
  -- Launch thread that puts lots of Bs
  -- Put lots of As
  return ()
