module App.Component.HTML.Footer where

import Preamble hiding (div)

import CSS (flexShrink)
import Halogen.Bulma (container, hasTextCentered)
import Halogen.Bulma as B
import Halogen.HTML.CSS (style)
import Halogen.HTML.Extended (a, div, p_, strong_, text)
import Halogen.HTML.Extended as HH
import Halogen.HTML.Properties (class_, classes, href)

footer :: forall i p. HH.HTML i p
footer = HH.footer [class_ B.footer, style $ flexShrink 0] 
  [ div [classes [container, hasTextCentered]] 
    [ p_ 
      [ strong_ [text "Servant/Halogen 5 template project"] 
      , text " by "
      , a [href "https://github.com/Unisay"] [text "Yuriy Lazarev"] 
      , text ". The source code is licensed "
      , a [href "http://opensource.org/licenses/mit-license.php"] [text "MIT"]
      ]
    ]
  ]