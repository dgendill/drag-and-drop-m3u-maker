module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (windowToEventTarget)
import DOM.HTML.Types as DHT
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToEventTarget, documentToNonElementParentNode, elementToEventTarget)
import Data.Maybe (fromJust)
import DragAndDropToM3U (dropHandler)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

type Effects r = Eff (dom :: DOM, console :: CONSOLE) r

windowLoad :: forall e. Effects Unit -> Effects Unit
windowLoad fn = do
  w <- window
  addEventListener (EventType "load") (eventListener (const fn)) false (windowToEventTarget w)

main :: Effects Unit
main = windowLoad $ do
  doc <- DHT.htmlDocumentToDocument <$> (window >>= document)
  element <- unsafePartial $ fromJust <$> (getElementById (ElementId "dropzone") (documentToNonElementParentNode doc))
  textarea <- unsafePartial $ fromJust <$> (getElementById (ElementId "m3u") (documentToNonElementParentNode doc))
  addEventListener (EventType "drop") (eventListener (dropHandler (unsafeCoerce textarea))) false (elementToEventTarget element)
  addEventListener (EventType "dragover") (eventListener preventDefault) false (documentToEventTarget doc)
  log "Running..."
