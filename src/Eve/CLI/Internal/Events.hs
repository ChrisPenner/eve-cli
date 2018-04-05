{-# language RankNTypes #-}
module Eve.CLI.Internal.Events
  ( vtyTerminalEvents
  , onEvent
  , onKeypress
  , onMouseDown
  , onMouseUp
  , onResize
  , onPaste
  , onEvent_
  , onKeypress_
  , onMouseDown_
  , onMouseUp_
  , onResize_
  , onPaste_
  , Keypress(..)
  , MouseDown(..)
  , MouseUp(..)
  , Resize(..)
  , Paste(..)
  , LostFocus(..)
  , GainedFocus(..)
  ) where

import Eve
import Eve.CLI.Internal.State
import Data.ByteString

import Control.Monad
import Control.Monad.Trans
import Data.Typeable

import qualified Graphics.Vty as V

-- | Dispatches terminal events to eve
vtyTerminalEvents :: (Typeable m, Typeable m, MonadIO m, HasEvents s) => AppT s m ()
vtyTerminalEvents = do
  v <- getVty
  asyncEventProvider $ dispatchVtyEvents v

dispatchVtyEvents ::  V.Vty -> EventDispatcher -> IO ()
dispatchVtyEvents v dispatch = forever $ do
  evt <- V.nextEvent v
  dispatch evt
  case evt of
    V.EvKey k m -> dispatch (Keypress k m)
    V.EvMouseDown x y button mods -> dispatch (MouseDown x y button mods)
    V.EvMouseUp x y button -> dispatch (MouseUp x y button)
    V.EvResize width height -> dispatch (Resize width height)
    V.EvPaste bytes -> dispatch (Paste bytes)
    V.EvLostFocus ->  dispatch LostFocus
    V.EvGainedFocus ->  dispatch GainedFocus

data Keypress = Keypress V.Key [V.Modifier]
  deriving (Eq, Show)

data MouseDown = MouseDown Int Int V.Button [V.Modifier]
  deriving (Eq, Show)

data MouseUp = MouseUp Int Int (Maybe V.Button)
  deriving (Eq, Show)

data Resize = Resize Int Int
  deriving (Eq, Show)

data Paste = Paste ByteString
  deriving (Eq, Show)

data LostFocus = LostFocus
  deriving (Eq, Show)

data GainedFocus = GainedFocus
  deriving (Eq, Show)

genericListener :: (Typeable evt, Typeable m, MonadIO m, HasEvents s) => (evt -> AppT s m result) -> AppT s m ListenerId
genericListener actionF = addListener (void <$> actionF)

-- | React to a Event
onEvent :: (Typeable m, MonadIO m, HasEvents s) => (V.Event -> AppT s m result) -> AppT s m ListenerId
onEvent = genericListener

-- | React to a Keypress
onKeypress :: (Typeable m, MonadIO m, HasEvents s) => (Keypress -> AppT s m result) -> AppT s m ListenerId
onKeypress = genericListener

-- | React to a Mouse Down
onMouseDown :: (Typeable m, MonadIO m, HasEvents s) => (MouseDown -> AppT s m result) -> AppT s m ListenerId
onMouseDown = genericListener

-- | React to a Mouse Up
onMouseUp :: (Typeable m, MonadIO m, HasEvents s) => (MouseUp -> AppT s m result) -> AppT s m ListenerId
onMouseUp = genericListener

-- | React to a Terminal Resize
onResize :: (Typeable m, MonadIO m, HasEvents s) => (Resize -> AppT s m result) -> AppT s m ListenerId
onResize = genericListener

-- | React to a Paste
onPaste :: (Typeable m, MonadIO m, HasEvents s) => (Paste -> AppT s m result) -> AppT s m ListenerId
onPaste = genericListener

-- | React to a Event
onEvent_ :: (Typeable m, MonadIO m, HasEvents s) => (V.Event -> AppT s m result) -> AppT s m ()
onEvent_ = void . genericListener

-- | React to a Keypress
onKeypress_ :: (Typeable m, MonadIO m, HasEvents s) => (Keypress -> AppT s m result) -> AppT s m ()
onKeypress_ = void . genericListener

-- | React to a Mouse Down
onMouseDown_ :: (Typeable m, MonadIO m, HasEvents s) => (MouseDown -> AppT s m result) -> AppT s m ()
onMouseDown_ = void . genericListener

-- | React to a Mouse Up
onMouseUp_ :: (Typeable m, MonadIO m, HasEvents s) => (MouseUp -> AppT s m result) -> AppT s m ()
onMouseUp_ = void . genericListener

-- | React to a Terminal Resize
onResize_ :: (Typeable m, MonadIO m, HasEvents s) => (Resize -> AppT s m result) -> AppT s m ()
onResize_ = void . genericListener

-- | React to a Paste
onPaste_ :: (Typeable m, MonadIO m, HasEvents s) => (Paste -> AppT s m result) -> AppT s m ()
onPaste_ = void . genericListener
