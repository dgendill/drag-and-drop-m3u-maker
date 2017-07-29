module HTML where

import Control.Bind ((>>=))
import Control.Monad.Eff (Eff)
import Control.Semigroupoid ((<<<), (>>>))
import DOM (DOM)
import DOM.Event.Types (EventTarget)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window as Window
import DOM.Node.Types (Document, Element, Node)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartialBecause)
import Prelude (Unit, ($), pure, (<>), (<$>))
import Unsafe.Coerce (unsafeCoerce)

foreign import readyImpl :: forall e a. Fn1 (Eff (dom :: DOM | e) a) Unit
foreign import getElementsByClassNameImpl :: forall a. Fn3 String (a -> Maybe a) (Maybe a) (Maybe (Array Element))
foreign import getElementByIdImpl :: forall a. Fn3 String (a -> Maybe a) (Maybe a) (Maybe Element)
foreign import setInnerHTMLImpl :: forall a. Fn2 String Element Unit
foreign import prependChildImpl :: forall a. Fn2 Element Element Unit
foreign import setDataAttributeImpl :: forall a. Fn2 Element String Unit

document :: forall e. Eff (dom :: DOM | e) Document
document = window >>= Window.document >>= htmlDocumentToDocument >>> pure

ready :: forall e a. Eff (dom :: DOM | e) a -> Eff (dom :: DOM | e) Unit
ready fn = pure $ runFn1 readyImpl fn

getElementById :: forall e. String -> Eff (dom :: DOM | e) (Maybe Element)
getElementById s = pure $ runFn3 getElementByIdImpl s Just Nothing

getElementsByClassName :: forall e. String -> Eff (dom :: DOM | e) (Maybe (Array Element))
getElementsByClassName s = pure $ runFn3 getElementsByClassNameImpl s Just Nothing

unsafeGetElementById :: forall e. String -> Eff (dom :: DOM | e) Element
unsafeGetElementById s = unsafePartialBecause ("#" <> s <> " could not be found") $ fromJust <$> getElementById s

toEventTarget :: Element -> EventTarget
toEventTarget = unsafeCoerce

toNode :: Element -> Node
toNode = unsafeCoerce

toElement :: Node -> Element
toElement = unsafeCoerce

setInnerHTML :: forall e. Element -> String -> Eff (dom :: DOM | e) Unit
setInnerHTML e s = pure $ runFn2 setInnerHTMLImpl s e

prependChild :: forall e. Element -> Element -> Eff (dom :: DOM | e) Unit
prependChild p c = pure $ runFn2 prependChildImpl p c

setDataAttribute :: forall e. Element -> String -> Eff (dom :: DOM | e) Unit
setDataAttribute e s = pure $ runFn2 setDataAttributeImpl e s