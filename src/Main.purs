module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Console (error) as Console
import Control.Monad.Eff.Exception (EXCEPTION, catchException, error, throwException)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)
import Control.Monad.State.Class (get)
import Control.Monad.State.Trans (StateT(..), execStateT, runStateT)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event, EventType(..))
import DOM.File.Types (File)
import DOM.HTML (window)
import DOM.HTML.HTMLTextAreaElement (setValue)
import DOM.HTML.Types (windowToEventTarget)
import DOM.HTML.Types as DHT
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Document, Element, ElementId(..), documentToEventTarget, documentToNonElementParentNode, elementToEventTarget)
import Data.Array (unsafeIndex)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Maybe (Maybe(..))
import Data.Maybe (fromJust)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)
import DragAndDropToM3U (dropHandler, foldToM3U, m3uDropHandler)
import HTML (setInnerHTML)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

type EffRows e = (dom :: DOM, console :: CONSOLE, exception :: EXCEPTION | e )
type Effects e r = Eff (EffRows e)  r

windowLoad :: forall e. Effects e Unit -> Effects e Unit
windowLoad fn = do
  w <- window
  addEventListener (EventType "load") (eventListener (const fn)) false (windowToEventTarget w)

main :: forall e. Effects e Unit
main = run {
  elements : defaultElementConfig,
  ops : defaultFunctionalityConfig
}

run :: forall e. Config (EffRows e) Unit -> Effects e Unit
run config = windowLoad $ do
  doc <- DHT.htmlDocumentToDocument <$> (window >>= document)
  ec@{ dropzone, textarea, errors } <- validateElementConfig config.elements doc
  addEventListener (EventType "drop") (eventListener
    (\event -> (execStateT (config.ops.processFiles event) ec)))
    false
    (elementToEventTarget dropzone)
  
  addEventListener (EventType "dragover") (eventListener preventDefault) false (documentToEventTarget doc)
  log "Running..."

type Config e a = {
  elements :: ElementConfig String,
  ops :: FunctionalityConfig e a
}

type FunctionalityConfig e a = {
  processFiles :: Event -> StateT (ElementConfig Element) (Eff e) a
}

type ElementConfig a = {
  dropzone :: a,
  textarea :: a,
  errors :: a
}

extendOps :: forall e. FunctionalityConfig (EffRows e) Unit -> FunctionalityConfig (EffRows e) Unit
extendOps ext = {
  processFiles : (\event -> do
    defaultFunctionalityConfig.processFiles event
    ext.processFiles event
  )
}

customFunction :: forall e. (Fn2 (ElementConfig Element) (Array File) Unit) -> FunctionalityConfig (dom :: DOM, console :: CONSOLE | e) Unit
customFunction fn = {
  processFiles : (\event -> do
    ec@{ dropzone, textarea, errors } <- get
    lift $ catchException (\err ->
      Console.error (message err)
    ) $ void $ runAff
      (const $ pure unit)
      (const $ pure unit)
      (dropHandler event ((\files -> pure $ runFn2 fn ec files )))
  )
}

defaultFunctionalityConfig :: forall e. FunctionalityConfig (console :: CONSOLE, dom :: DOM | e) Unit
defaultFunctionalityConfig = {
  processFiles : (\event -> do
    { dropzone, textarea, errors } <- get
    let errorElement = errors
    lift $ void $ runAff  (\err -> do
      setInnerHTML errorElement (message err)
    ) (\str -> do
      setValue str (unsafeCoerce textarea)
      setInnerHTML errorElement ""
    ) $ m3uDropHandler (unsafeCoerce textarea) errors event
  )
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