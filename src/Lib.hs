module Lib
    ( jsonToSvg
    ) where

import Text.XML
import Svg
import Svg.Lesions
import Json

jsonToSvg :: Fundus -> Document
jsonToSvg input = fundusDrawing eye elements
    where eye      = case (eyeSide input) of
                        "RightEye" -> RightEye
                        "LeftEye"  -> LeftEye
          elements = map parseTear $ tears input
          parseTear (_, clock, ecc) = tearElement clock ecc
