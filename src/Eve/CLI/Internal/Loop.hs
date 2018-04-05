module Eve.CLI.Internal.Loop
  ( initCLI
  ) where

import Eve
import Eve.CLI.Internal.Events
import Eve.CLI.Internal.State
import qualified Graphics.Vty as V
import Control.Monad.Trans

initCLI :: HasEvents s => AppT s IO ()
initCLI = do
  vtyTerminalEvents
  onExit shutdown

-- | Call vty shutdown procedure (if this doesn't happen the terminal ends up in strange states)
shutdown :: HasEvents s => AppT s IO ()
shutdown = do
  v <- getVty
  liftIO $ V.shutdown v
