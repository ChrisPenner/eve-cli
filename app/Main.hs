module Main where

import Eve (eve_, App, exit)
import Eve.CLI (initCLI, onKeypress_, renderImage, Keypress(..))
import qualified Data.Text.Lazy as T
import qualified Graphics.Vty as V
import Control.Monad (void)

main :: IO ()
main = eve_ $ do
  initCLI
  onKeypress_ showKeypress
    where
      showKeypress :: Keypress -> App ()
      showKeypress (Keypress V.KEsc _) = exit
      showKeypress keypress = void . renderImage $ V.text V.defAttr . T.pack . show $ keypress
