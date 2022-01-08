module P5.Mouse where

import Prelude

import Effect (Effect)
import P5 (P5)

foreign import _mouseX :: P5 -> Effect Number
foreign import _mouseY :: P5 -> Effect Number

mouseX :: P5 -> Effect Number
mouseX = _mouseX

mouseY :: P5 -> Effect Number
mouseY = _mouseY
