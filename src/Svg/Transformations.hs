{-# LANGUAGE OverloadedStrings #-}

module Svg.Transformations (transformElement
                           , rotateTransformation
                           , translateTransformation
                           , rotateElement
                           , translateElement
                           , mirrorElement) 
    where

import Text.XML
import qualified Data.Map  as M
import qualified Data.Text as T
import Svg.Basic

transformElement :: Element -> T.Text -> Element
transformElement e transformation = e { elementAttributes = M.insertWith bindFunction "transform" transformation oldAttributes }
  where oldAttributes = elementAttributes e
        bindFunction old new = T.intercalate " " [ old, new ]

rotateTransformation :: Clock -> T.Text
rotateTransformation clock = T.pack $ "rotate(" ++ (show $ clockToDegree clock) ++ " " ++ centerX ++ " " ++ centerY ++ ")" 

translateTransformation :: Eccentricity -> T.Text
translateTransformation ecc = T.pack $ "translate(" ++ "0  -" ++ (show $ eccentricityToPixels ecc) ++ ")"

rotateElement :: Clock -> Element -> Element
rotateElement clock e = transformElement e $ rotateTransformation clock

translateElement :: Eccentricity -> Element -> Element
translateElement ecc el = transformElement el $ translateTransformation ecc

mirrorElement :: Element -> Element
mirrorElement e = e { elementAttributes = M.insert "transform-origin" "center" $ M.insert "transform" "scale (-1,1)" $ elementAttributes e }