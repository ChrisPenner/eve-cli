Eve.CLI
=======

Eve.CLI provides [`eve`](https://github.com/ChrisPenner/eve) compatible helpers for building CLI apps.
It allows you to:
- Respond to Keyboard, Mouse, and Resize Events
- Render text/images to your terminal

Here's what it looks like:

```haskell
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
      -- | Display the last key combination that you pressed on screen
      showKeypress :: Keypress -> App ()
      showKeypress (Keypress V.KEsc _) = exit
      showKeypress keypress = void . renderImage $ V.text V.defAttr . T.pack . show $ keypress
```

Events
------

Eve.CLI is a small wrapper on top of [vty](https://github.com/jtdaugherty/vty); so you'll also need to import
`Graphics.Vty` in order to interact with most events. The following event listeners are provided:

- onEvent
- onKeypress
- onMouseDown
- onMouseUp
- onResize
- onPaste

See the hackage docs for more in depth API documentation.

Rendering
---------

Currently Eve.CLI supports only rendering a
[`Vty.Image`](http://hackage.haskell.org/package/vty-5.21/docs/Graphics-Vty-Image.html);
this means you can use any of `Vty`'s image building combinators, then simply
call `renderImage` with the image you've built.
