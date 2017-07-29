module DragAndDropToM3U where

import Prelude

import Control.Monad.Aff (Aff, launchAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (message)
import DOM (DOM)
import DOM.Event.Event (Event, preventDefault)
import DOM.File.File (name)
import DOM.File.Types (File, FileList)
import DOM.HTML.Event.DataTransfer (files)
import DOM.HTML.Event.DragEvent (dataTransfer)
import DOM.HTML.Event.Types (DragEvent)
import DOM.HTML.HTMLTextAreaElement (setValue)
import DOM.HTML.Types (HTMLAudioElement, HTMLElement, HTMLTextAreaElement)
import DOM.Node.Types (Element)
import Data.Array (foldM)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Int (ceil)
import Data.Maybe (Maybe(..))
import HTML (setInnerHTML)
import Unsafe.Coerce (unsafeCoerce)

newtype URL = URL String

newtype AudioTags = AudioTags {
  title :: String,
  artist :: String,
  filename :: String
}

type Effects e = (dom :: DOM, console :: CONSOLE | e)

foreign import fileUrlImpl :: Fn1 File String
fileUrl :: File -> String
fileUrl = runFn1 fileUrlImpl

foreign import injectAudioHiddenImpl :: forall e. Fn2 String File HTMLAudioElement
injectAudioHidden :: forall e. String -> File -> (Eff (Effects e) HTMLAudioElement)
injectAudioHidden s f = pure $ runFn2 injectAudioHiddenImpl s f

foreign import audioTagsImpl :: forall e. File -> Aff (e) AudioTags
audioTags ::  forall e. File -> Aff (e) AudioTags
audioTags = runFn1 audioTagsImpl

foreign import audioDurationImpl :: forall e. Fn1 HTMLAudioElement (Aff (Effects e) Number)
audioDuration :: forall e. HTMLAudioElement -> Aff (Effects e) Number
audioDuration = runFn1 audioDurationImpl

foreign import toFileArrayImpl :: Fn1 FileList (Array File)
toFileArray :: FileList -> Array File
toFileArray = runFn1 toFileArrayImpl

m3uStart :: String
m3uStart = "#EXTM3U\n"

filesToM3U :: forall e. DragEvent -> Aff (Effects e) String
filesToM3U event = do
  let mfiles = toFileArray <$> (files $ dataTransfer event)
  case mfiles of
    Just files -> do
      append m3uStart <$> foldM (\acc file -> do
        let url = fileUrl file
        audioElement <- liftEff $ injectAudioHidden (fileUrl file) file
        duration <- audioDuration audioElement
        (AudioTags {title, artist, filename}) <- audioTags file
        pure $ acc <>
          "#EXTINF:" <> (show $ ceil duration) <> ", " <> artist <> " - " <> title <> "\n" <>
          (name file) <> "\n"
      ) "" files

    Nothing -> pure ""


dropHandler :: forall e. HTMLTextAreaElement -> Element -> Event -> Eff (Effects e) Unit
dropHandler textarea errorElement e = do
  preventDefault e
  void $ runAff (\err -> do
    setInnerHTML errorElement (message err)
  ) (\str -> do
    setValue str textarea
    setInnerHTML errorElement ""
  ) $ filesToM3U (unsafeCoerce e)
