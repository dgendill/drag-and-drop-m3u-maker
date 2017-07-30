module Main where

import Prelude

import Control.Coroutine (Producer, await, pullFrom, runProcess)
import Control.Coroutine.Aff (produce, produce')
import Control.Monad.Aff (Aff, attempt, launchAff, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Console (error) as Console
import Control.Monad.Eff.Exception (EXCEPTION, catchException, error, throwException)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Class (get, modify, put)
import Control.Monad.State.Trans (StateT(..), execStateT, runStateT)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event, EventTarget, EventType(..))
import DOM.File.Types (File)
import DOM.HTML (window)
import DOM.HTML.HTMLTextAreaElement (setValue)
import DOM.HTML.Types (windowToEventTarget)
import DOM.HTML.Types as DHT
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Document, Element, ElementId(..), documentToEventTarget, documentToNonElementParentNode, elementToEventTarget)
import Data.Array (unsafeIndex)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Data.Maybe (fromJust)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)
import DragAndDropToM3U (dropHandler, eventToFiles, fileUrl, foldToM3U, m3uDropHandler)
import HTML (setInnerHTML)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

type EffRows e = (dom :: DOM, console :: CONSOLE, exception :: EXCEPTION, avar :: AVAR | e )
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

type AppState = { elements :: (ElementConfig Element), files :: Array File }
type AppEffects e = StateT AppState (Aff (EffRows e))

dropEvents :: forall e. EventType -> EventTarget -> Producer Event (AppEffects e) Unit
dropEvents eventType target = produce' \emit ->
  addEventListener eventType (eventListener (emit <<< Left))
    false
    target

setupListener :: forall e. Element -> (Array File -> AppEffects e Unit) -> AppEffects e Unit
setupListener dropzone ondrop = runProcess $ consumer `pullFrom` producer
  where
  producer =
    dropEvents
      (EventType "drop")
      (elementToEventTarget dropzone)
  consumer = forever $ lift <<< (\event -> do
    liftEff $ preventDefault event
    let files = eventToFiles (unsafeCoerce event)
    ondrop files
  ) =<< await

run :: forall e. Config e Unit -> Effects e Unit
run config = windowLoad $ do
  doc <- DHT.htmlDocumentToDocument <$> (window >>= document)
  ec@{ dropzone, textarea, errors } <- validateElementConfig config.elements doc

  void $ runAff
    (const $ pure unit)
    (const $ pure unit) $ { elements : ec, files : [] } # execStateT do    
    lift $ liftEff $ addEventListener
      (EventType "dragover")
      (eventListener preventDefault)
      false
      (documentToEventTarget doc)

    setupListener dropzone config.ops.processFiles

  
  log "Running..."

type Config e a = {
  elements :: ElementConfig String,
  ops :: FunctionalityConfig e a
}

type FunctionalityConfig e a = {
  processFiles :: Array File -> AppEffects e a
}

type ElementConfig a = {
  dropzone :: a,
  textarea :: a,
  errors :: a
}

extendOps :: forall e. FunctionalityConfig e Unit -> FunctionalityConfig e Unit
extendOps ext = {
  processFiles : (\files -> do
    defaultFunctionalityConfig.processFiles files
    ext.processFiles files
  )
}

customFunction :: forall e. (Fn3 (ElementConfig Element) (Array File) (Array File) Unit) -> FunctionalityConfig e Unit
customFunction fn = {
  processFiles : (\newFiles -> do
    { elements, files } <- get
    void $ lift $ pure $ runFn3 fn elements newFiles files
  )
}

defaultFunctionalityConfig :: forall e. FunctionalityConfig e Unit
defaultFunctionalityConfig = {
  processFiles : (\newFiles -> do
    { elements, files } <- get
    let errorElement = elements.errors    
    let allFiles = files <> newFiles  
    m3u <- lift $ attempt $ foldToM3U allFiles
    case m3u of
      Left err -> liftEff $ setInnerHTML errorElement (message err)
      Right str -> do
        liftEff $ setValue str (unsafeCoerce elements.textarea)
        liftEff $ setInnerHTML errorElement ""
        modify (\s -> (s { files = allFiles }))
        
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