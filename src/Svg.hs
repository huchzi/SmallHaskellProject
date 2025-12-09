{-# LANGUAGE OverloadedStrings #-}

module Svg (fundusDrawing
           , rotateElement
           , Eye(..)
           , RetinalZone(..)
           , eccentricity
           ) 
    where

import Svg.Basic
import Svg.Transformations
import Svg.Template
import Text.XML
import qualified Data.Map  as M
import qualified Data.Text as T

data RetinalZone
    = Macula
    | Parafoveal
    | PosteriorPole
    | Posterior
    | Equatorial
    | PreEquatorial
    | Anterior
    | OraSerrata
    deriving (Show, Eq, Ord, Enum, Bounded)

eccentricity :: RetinalZone -> Eccentricity
eccentricity Macula        = 0
eccentricity Parafoveal    = 10
eccentricity PosteriorPole = 30
eccentricity Posterior     = 60
eccentricity Equatorial    = 90
eccentricity PreEquatorial = 100
eccentricity Anterior      = 110
eccentricity OraSerrata    = 120

-- Fundus Template

svgName :: Name
svgName = "{http://www.w3.org/2000/svg}svg"

fundusDocAttributes :: M.Map Name T.Text
fundusDocAttributes = 
  (M.fromList [ ("width", "400")
              , ("height", "400")
              ])

fundusDrawing :: Eye -> [Element] -> Document
fundusDrawing eye elements =
  Document
    (Prologue [] Nothing [])
    (Element svgName fundusDocAttributes $ map NodeElement ((fundusTemplate eye):elements))
    []                                         