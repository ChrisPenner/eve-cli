module Eve.CLI.Internal.Render
  ( renderImage
  , Width
  , Height
  , getSize
  ) where

import Eve
import Eve.CLI.Internal.State
import Graphics.Vty as V
import Control.Monad.Trans

-- | Render the provided 'V.image' to screen
renderImage :: (MonadIO m, HasStates s) => V.Image -> AppT s m ()
renderImage img = do
  v <- getVty
  liftIO $ V.update v (V.picForImage img)

-- | Terminal width in columns
type Width = Int
-- | Terminal height in rows
type Height = Int


-- | Get the current terminal size.
-- Also see 'onResize'
getSize :: (MonadIO m, HasStates s) => AppT s m (Width, Height)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v
