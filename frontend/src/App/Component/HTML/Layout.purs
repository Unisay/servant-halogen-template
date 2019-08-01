module App.Component.HTML.Layout where

import Preamble hiding (div)

import App.Component.HTML.Footer (footer)
import CSS (column, display, flex, flexDirection, height, pct, flexBasis, flexGrow, flexShrink)
import CSS.Common (auto)
import Halogen.Bulma as B
import Halogen.HTML.CSS (style)
import Halogen.HTML.Extended (HTML, div, section)
import Halogen.HTML.Properties (class_)


main :: forall i p. HTML i p -> Array (HTML i p) -> HTML i p
main header content = 
  div 
    [ pageStyle ] 
    [ header
    , section [class_ B.section, contentStyle] content
    , footer
    ]
  where

  pageStyle = style do
    height (pct 100.0)
    display flex
    flexDirection column

  contentStyle = style do
    flexGrow 1
    flexShrink 0
    flexBasis auto