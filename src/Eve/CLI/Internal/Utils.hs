module Eve.CLI.Internal.Utils
  ( getSize
  ) where

import Eve
import Eve.CLI.Internal.State
import Eve.CLI.Internal.Types
import Graphics.Vty as V
import Control.Monad.Trans

-- | Get the current terminal size.
getSize :: App (Width, Height)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v
