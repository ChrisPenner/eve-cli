module Eve.CLI.Internal.Render
  ( renderImage
  ) where

import Eve
import Eve.CLI.Internal.State
import Graphics.Vty as V
import Control.Monad.Trans

renderImage :: (MonadIO m, HasStates s) => V.Image -> AppT s m ()
renderImage img = do
  v <- getVty
  liftIO $ V.update v (V.picForImage img)
