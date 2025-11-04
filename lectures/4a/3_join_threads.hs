import Control.Concurrent
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  --Two threads, each says their name, then we join them
  return ()
