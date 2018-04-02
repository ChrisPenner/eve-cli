module Eve.CLI.Internal.State where

import Eve
import Control.Lens
import qualified Graphics.Vty as V

import Control.Monad.Trans

-- | Store 'V.Vty' state globally
newtype VTY = VTY V.Vty

-- | V.Vty must be initialized inside IO.
initUi :: App V.Vty
initUi = do
  cfg <- liftIO V.standardIOConfig
  v <- liftIO $ V.mkVty cfg
  stateLens .= Just (VTY v)
  return v

-- | Gets vty by checking if it has been initialized yet, if not it runs the initialization.
getVty :: App V.Vty
getVty = do
  v <- use stateLens
  case v of
    Just (VTY v') -> return v'
    Nothing -> initUi
