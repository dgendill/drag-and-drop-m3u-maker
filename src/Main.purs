module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (windowToEventTarget)
import DOM.HTML.Types as DHT
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Document, Element, ElementId(..), documentToEventTarget, documentToNonElementParentNode, elementToEventTarget)
import Data.Array (unsafeIndex)
import Data.Maybe (Maybe(..))
import Data.Maybe (fromJust)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)
import DragAndDropToM3U (dropHandler)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

type Effects r = Eff (dom :: DOM, console :: CONSOLE, exception :: EXCEPTION) r

windowLoad :: forall e. Effects Unit -> Effects Unit
windowLoad fn = do
  w <- window
  addEventListener (EventType "load") (eventListener (const fn)) false (windowToEventTarget w)

main :: Effects Unit
main = run defaultElementConfig

run :: ElementConfig String -> Effects Unit
run config = windowLoad $ do
  doc <- DHT.htmlDocumentToDocument <$> (window >>= document)
  { dropzone, textarea, errors } <- validateElementConfig config doc
  addEventListener (EventType "drop") (eventListener (dropHandler (unsafeCoerce textarea) errors)) false (elementToEventTarget dropzone)
  addEventListener (EventType "dragover") (eventListener preventDefault) false (documentToEventTarget doc)
  log "Running..."

type FunctionalityConfig = {
  
}

type ElementConfig a = {
  dropzone :: a,
  textarea :: a,
  errors :: a
}

defaultElementConfig :: ElementConfig String
defaultElementConfig = {
  dropzone : "dropzone",
  textarea : "m3u",
  errors : "errors"
}

validateElementConfig :: forall e. ElementConfig String -> Document -> Eff (dom :: DOM, exception :: EXCEPTION | e) (ElementConfig Element)
validateElementConfig config doc =
  unV throwError arrToElements <$> (validateElements <$> checkElements)
  where

  arrToElements :: Array Element -> ElementConfig Element
  arrToElements arr = {
    dropzone : unsafePartial unsafeIndex arr 0,
    textarea : unsafePartial unsafeIndex arr 1,
    errors : unsafePartial unsafeIndex arr 2
  }

  throwError :: forall a. Array String -> a
  throwError errs =
    unsafeThrowException (error $ joinWith " " errs)

  validateElements :: forall e. Array (Tuple (Maybe Element) String) -> V (Array String) (Array Element)
  validateElements =
    traverse \(Tuple melement message) -> do
      case melement of
        Just element -> pure element
        Nothing -> invalid [message]

  checkElements :: forall e. Eff (dom :: DOM | e) (Array (Tuple (Maybe Element) String))
  checkElements = do
    let docpn = documentToNonElementParentNode doc
    element <- getElementById (ElementId config.dropzone) docpn
    textarea <- getElementById (ElementId config.textarea) docpn
    errors <- getElementById (ElementId config.errors) docpn
    pure $ [
      Tuple element ("config.dropzone of '" <> config.dropzone <> "' could not be found on the page. It should be an ID."),
      Tuple textarea ("config.textarea of '" <> config.textarea <> "' could not be found on the page.  It should be an ID."),
      Tuple errors ("config.errors of '" <> config.dropzone <> "' could not be found on the page.  It should be an ID.")
    ]