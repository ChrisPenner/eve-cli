module Main where

import Eve
import Eve.CLI
import qualified Data.Text.Lazy as T
import Graphics.Vty as V
import Control.Monad

main :: IO ()
main = runCLI $ do
  onKeypress_ showKeypress
    where
      showKeypress :: Keypress -> App ()
      showKeypress (Keypress V.KEsc _) = exit
      showKeypress keypress = void . renderImage $ V.text V.defAttr . T.pack . show $ keypress
