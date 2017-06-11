module DragAndDropToM3U where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML.Types (HTMLAudioElement, HTMLElement)

newtype URL = URL String

foreign import data File :: Type

foreign import fileUrl :: File -> String
foreign import injectAudio :: String -> File -> HTMLElement -> Eff (dom :: DOM) HTMLAudioElement

foreign import audioDuration :: HTMLAudioElement -> Aff (dom :: DOM) Number

m3uStart :: String
m3uStart = "#EXTM3U\n"
