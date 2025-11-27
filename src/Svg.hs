{-# LANGUAGE OverloadedStrings #-}

module Svg (fundusTemplate) where

import Text.XML
import qualified Data.Map        as M

svgName :: Name
svgName = "{http://www.w3.org/2000/svg}svg"

fundusDocAttributes = 
  (M.fromList [ ("width", "400")
              , ("height", "400")
              ])

-- Template Elements
outerCircle :: Element
outerCircle = Element "{http://www.w3.org/2000/svg}circle" (M.fromList [ ("cx", "200")
                                                                       , ("cy", "200")
                                                                       , ("r", "190")
                                                                       , ("fill", "red")
                                                                       , ("style", "fill:none;stroke:black;stroke-width:2px;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;")
                                                                       ]) []

equatorCircle :: Element
equatorCircle = Element "{http://www.w3.org/2000/svg}circle" (M.fromList [ ("cx", "200")
                                                                       , ("cy", "200")
                                                                       , ("r", "95")
                                                                       , ("fill", "red")
                                                                       , ("style", "fill:none;stroke:black;stroke-width:2px;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;")
                                                                       ]) []

oraCircle :: Element
oraCircle = Element "{http://www.w3.org/2000/svg}circle" (M.fromList [ ("cx", "200")
                                                                       , ("cy", "200")
                                                                       , ("r", "142")
                                                                       , ("fill", "red")
                                                                       , ("style", "fill:none;stroke:black;stroke-width:2px;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;")
                                                                       ]) []

-- Fundus Template
fundusTemplate :: Document
fundusTemplate =
  Document
    (Prologue [] Nothing [])
    (Element svgName fundusDocAttributes [ NodeElement outerCircle
                                         , NodeElement equatorCircle
                                         , NodeElement oraCircle
                                         ])
    []
