module Eve.CLI.Internal.Loop where

import Eve
import Eve.CLI.Internal.Events
import Eve.CLI.Internal.State
import qualified Graphics.Vty as V
import Control.Monad.Trans

runCLI :: App () -> IO ()
runCLI listeners = eve_ $ do
  vtyTerminalEvents
  onExit shutdown
  listeners

-- | Call vty shutdown procedure (if this doesn't happen the terminal ends up in strange states)
shutdown :: App ()
shutdown = do
  v <- getVty
  liftIO $ V.shutdown v
