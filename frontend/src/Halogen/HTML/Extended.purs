module Halogen.HTML.Extended 
  ( module HH
  , (<?.>)
  , css
  , className
  , dataAttr
  , maybeElem
  , maybeZero
  , runStaticHtml
  , StaticHTML
  , safeHref
  , whenElem
  ) where

import Preamble

import App.Data.Route (Route, routeCodec)
import Control.MonadZero (class MonadZero)
import Control.Plus (empty)
import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.HTML (class IsProp, AttrName(..), ClassName(..), ComponentHTML, ElemName(..), HTML(..), IProp, Leaf, Namespace(..), Node, PlainHTML, PropName(..), a, a_, abbr, abbr_, address, address_, area, article, article_, aside, aside_, attr, attrNS, audio, audio_, b, b_, base, bdi, bdi_, bdo, bdo_, blockquote, blockquote_, body, body_, br, br_, button, button_, canvas, caption, caption_, cite, cite_, code, code_, col, colgroup, colgroup_, command, datalist, datalist_, dd, dd_, del, del_, details, details_, dfn, dfn_, dialog, dialog_, div, div_, dl, dl_, dt, dt_, element, elementNS, em, em_, embed, embed_, fieldset, fieldset_, figcaption, figcaption_, figure, figure_, footer, footer_, form, form_, fromPlainHTML, h1, h1_, h2, h2_, h3, h3_, h4, h4_, h5, h5_, h6, h6_, handler, head, head_, header, header_, hr, hr_, html, html_, i, i_, iframe, img, input, ins, ins_, kbd, kbd_, keyed, keyedNS, label, label_, legend, legend_, li, li_, link, main, main_, map, map_, mark, mark_, memoized, menu, menu_, menuitem, menuitem_, meta, meter, meter_, nav, nav_, noscript, noscript_, object, object_, ol, ol_, optgroup, optgroup_, option, option_, output, output_, p, p_, param, pre, pre_, progress, progress_, prop, q, q_, rp, rp_, rt, rt_, ruby, ruby_, samp, samp_, script, script_, section, section_, select, select_, slot, small, small_, source, span, span_, strong, strong_, style, style_, sub, sub_, summary, summary_, sup, sup_, table, table_, tbody, tbody_, td, td_, text, textarea, tfoot, tfoot_, th, th_, thead, thead_, time, time_, title, title_, tr, tr_, track, u, u_, ul, ul_, var, var_, video, video_, wbr, withKeys, withKeys_) as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (print)
import Halogen.HTML.Core (AttrName(..)) as HC


-- | HTML written in Purescript via Halogen's HTML DSL
-- | that is always rendered the same and does not include any event handling.
type StaticHTML = H.ComponentHTML Unit () Aff

-- | Renders the static HTML once the body element becomes available.
runStaticHtml :: StaticHTML -> Effect Unit
runStaticHtml staticHTML = do
  launchAff_ do
    body <- awaitBody
    runUI (staticComponent staticHTML) unit body

-- | Wraps Halogen types cleanly, so that one gets very clear compiler errors
staticComponent :: StaticHTML
                -> H.Component HH.HTML (Const Unit) Unit Void Aff
staticComponent staticHtml =
  H.mkComponent
    { initialState: const unit
    , render: \_ -> staticHtml
    , eval: H.mkEval H.defaultEval
    }

-- | I get annoyed writing `class_ $ ClassName "..."` over and over again. This small utility saves
-- | a few characters all over our HTML.
css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

-- | We must provide a `String` to the "href" attribute, but we represent routes with the much
-- | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.
safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec

-- | Sometimes we need to deal with elements which may or may not exist. 
-- | This function lets us provide rendering for the element if it exists, 
-- | and renders an empty node otherwise.
maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

-- | PureScript is a strict language. If we want to conditionally display 
-- | an element, then we should hide the evaluation behind a function, 
-- | which won't be evaluated right away, in order to minimize the work 
-- | performed each render.
whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem true f = f unit 
whenElem _ _ = HH.text ""

maybeZero :: ∀ a b m . MonadZero m => Maybe a -> (a -> b) -> m b
maybeZero mb f = maybe empty (f >>> pure) mb

infix 2 maybeZero as <?.>

className :: forall a r. String -> HH.IProp (class :: String | r) a
className name = HP.class_ (HH.ClassName name)

dataAttr :: forall p i. String -> String -> HH.IProp p i
dataAttr name = HP.attr (HC.AttrName $ "data-" <> name)