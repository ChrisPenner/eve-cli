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
import Data.Typeable

import qualified Graphics.Vty as V

-- | Dispatches terminal events to eve
vtyTerminalEvents :: App ()
vtyTerminalEvents = do
  v <- getVty
  asyncEventProvider $ dispatchVtyEvents v

dispatchVtyEvents :: V.Vty -> EventDispatcher -> IO ()
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

genericListener :: Typeable evt => (evt -> App result) -> App ListenerId
genericListener actionF = addListener (void <$> actionF)

-- | React to a Event
onEvent :: (V.Event -> App result) -> App ListenerId
onEvent = genericListener

-- | React to a Keypress
onKeypress :: (Keypress -> App result) -> App ListenerId
onKeypress = genericListener

-- | React to a Mouse Down
onMouseDown :: (MouseDown -> App result) -> App ListenerId
onMouseDown = genericListener

-- | React to a Mouse Up
onMouseUp :: (MouseUp -> App result) -> App ListenerId
onMouseUp = genericListener

-- | React to a Terminal Resize
onResize :: (Resize -> App result) -> App ListenerId
onResize = genericListener

-- | React to a Paste
onPaste :: (Paste -> App result) -> App ListenerId
onPaste = genericListener

-- | React to a Event
onEvent_ :: (V.Event -> App result) -> App ()
onEvent_ = void . genericListener

-- | React to a Keypress
onKeypress_ :: (Keypress -> App result) -> App ()
onKeypress_ = void . genericListener

-- | React to a Mouse Down
onMouseDown_ :: (MouseDown -> App result) -> App ()
onMouseDown_ = void . genericListener

-- | React to a Mouse Up
onMouseUp_ :: (MouseUp -> App result) -> App ()
onMouseUp_ = void . genericListener

-- | React to a Terminal Resize
onResize_ :: (Resize -> App result) -> App ()
onResize_ = void . genericListener

-- | React to a Paste
onPaste_ :: (Paste -> App result) -> App ()
onPaste_ = void . genericListener
