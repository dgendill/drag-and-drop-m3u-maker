module DragAndDropToM3U where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Control.Fold (Fold)
import Control.Fold as Fold
import Control.Monad.Aff (Aff, launchAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (message)
import Control.Monad.List.Trans (foldlRec, singleton)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec)
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
import Data.Array (foldM, fromFoldable, head, tail)
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Int (ceil)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
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


foldFiles :: forall a b f. (Foldable f) => (a -> File -> a) -> a -> f File -> a
foldFiles fn seed f = tailRec go { acc : seed, val : (fromFoldable f) }
  where
  go :: _ -> Step _ a  
  go {acc : a, val : files } = case Tuple (head files) (tail files) of
    Tuple (Just h) (Just t) -> Loop $ { acc : fn a h, val : t }
    Tuple (Just h) Nothing  -> Done $ fn a h
    _ -> Done a

foldToM3U :: forall e. Array File -> Aff (Effects e) String
foldToM3U files = append m3uStart <$> foldM (\acc file -> do
  let url = fileUrl file
  audioElement <- liftEff $ injectAudioHidden (fileUrl file) file
  duration <- audioDuration audioElement
  (AudioTags {title, artist, filename}) <- audioTags file
  pure $ acc <>
    "#EXTINF:" <> (show $ ceil duration) <> ", " <> artist <> " - " <> title <> "\n" <>
    (name file) <> "\n"
) "" files

m3uStart :: String
m3uStart = "#EXTM3U\n"

execFiles :: forall e a. (Monoid a) => DragEvent -> (Array File -> Aff (dom :: DOM | e) a) -> Aff (dom :: DOM | e) a
execFiles event fn = do
  let mfiles = toFileArray <$> (files $ dataTransfer event)
  case mfiles of
    Just files -> fn files
    Nothing -> pure mempty

dropHandler :: forall e a. (Monoid a) => Event -> (Array File -> Aff (dom :: DOM | e) a) -> Aff (dom :: DOM | e) a
dropHandler e fn = do
  liftEff $ preventDefault e
  execFiles (unsafeCoerce e) fn

m3uDropHandler :: forall e. HTMLTextAreaElement -> Element -> Event -> Aff (Effects e) String
m3uDropHandler textarea errorElement e = do
  dropHandler e foldToM3U

