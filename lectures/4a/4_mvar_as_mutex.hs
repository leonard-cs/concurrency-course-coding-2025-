import Control.Concurrent
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  -- Two threads, say who they are over multiple print statements,
  -- using protectedPrint
