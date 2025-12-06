{-# LANGUAGE OverloadedStrings #-}

module Svg.Lesions (latticeElement
                  , tearElement
                  , roundHoleElement
                  , cobbleStoneElement) 
    where

import Text.XML
import qualified Data.Map  as M
import qualified Data.Text as T
import Svg.Basic
import Svg.Transformations

-- Lesion Elements

tearPath :: T.Text
tearPath = T.pack $ "M" ++ leftBorder ++ "," ++ ypos ++ " A" ++ gr ++ "," ++ gr ++ " 0 0,0 " ++ rightBorder ++ "," ++ ypos  ++ " A" ++ showSR ++ "," ++ showSR ++ " 0 0,1 " ++ leftBorder ++ "," ++ ypos ++ " Z"
  where sr = 15 :: Int
        gr = "20"
        xpos = 200 :: Int
        ypos = centerY
        leftBorder = show (xpos - sr)
        rightBorder = show (xpos + sr)
        showSR = show sr

tearElement :: Eccentricity -> Element
tearElement ecc = translateElement ecc $ Element "{http://www.w3.org/2000/svg}path" attr []
  where attr =  (M.fromList [ ("d", tearPath)
                            , ("fill", "red")
                            , ("stroke", "blue")
                            , ("stroke-width", "2")])

roundHoleElement :: Eccentricity -> Element
roundHoleElement ecc = translateElement ecc $ Element "{http://www.w3.org/2000/svg}circle" attr []
  where attr =  (M.fromList [ ("r", "5")
                            , ("cx", "200")                                                         
                            , ("cy", "200")
                            , ("fill", "red")
                            , ("stroke", "blue")
                            , ("stroke-width", "2")])

cobbleStoneElement :: Eccentricity -> Element
cobbleStoneElement ecc = translateElement ecc $ Element "{http://www.w3.org/2000/svg}rect" attr []
  where attr =  (M.fromList [ ("x", "196")                                                         
                            , ("y", "196")
                            , ("width", "8")
                            , ("height", "8")
                            , ("fill", "none")
                            , ("stroke", "black")
                            , ("stroke-width", "2")])

circleDef :: Eccentricity -> Element
circleDef ecc = 
  Element
    "{http://www.w3.org/2000/svg}defs"
    mempty
    [ NodeElement $ Element "{http://www.w3.org/2000/svg}circle" attr [] ]
  where attr = M.fromList [ ("id", "circularPath")
                          , ("r", T.pack $ show $ eccentricityToPixels ecc)
                          , ("cx", T.pack centerX)
                          , ("cy", T.pack centerY)
                          ]

latticeArcs :: ClockAngle -> Int -> Element
latticeArcs clockDifference radius = 
  Element
    "{http://www.w3.org/2000/svg}g"
    mempty
    (map (NodeElement . arcElement) [innerArc, outerArc, startArc, stopArc])
  where arcElement arcPath = Element "{http://www.w3.org/2000/svg}path"  (M.fromList [ ("d", arcPath), ("stroke", "black"), ("fill", "none"), ("stroke-width", "2")]) []
        innerArc = T.intercalate "" ["M ", T.show x1, " ", T.show y1, " A ", T.show radius, " ", T.show radius, " 0 0 1 ", T.show x2, " ", T.show y2]
        outerArc = T.intercalate "" ["M ", T.show x3, " ", T.show y3, " A ", T.show (radius - 16), " ", T.show (radius - 16), " 0 0 1 ", T.show x4, " ", T.show y4]
        startArc = T.intercalate "" ["M ", T.show x1, " ", T.show y1, " A 8 8 0 0 0 ", T.show x3, " ", T.show y3]
        stopArc  = T.intercalate "" ["M ", T.show x2, " ", T.show y2, " A 8 8 0 0 1 ", T.show x4, " ", T.show y4]          
        (x2, y2) = polarToCartesian stopAngle radius
        (x1, y1) = polarToCartesian startAngle radius
        (x4, y4) = polarToCartesian stopAngle (radius - 16)
        (x3, y3) = polarToCartesian startAngle (radius - 16)
        startAngle = 270 - div (clockDifference * 360) 24
        stopAngle = 270 + div (clockDifference * 360) 24

latticeElement :: ClockAngle -> Eccentricity -> Element
latticeElement clockDifference ecc = 
  Element
    "{http://www.w3.org/2000/svg}g"
    mempty
    [ NodeElement $ circleDef ecc
    , NodeElement $ textElement 
    , NodeElement $ latticeArcs clockDifference (eccentricityToPixels ecc) 
    ]
  where textElement = Element "{http://www.w3.org/2000/svg}text" (M.fromList [("font-family", "sans-serif"), ("font-size", "20")]) [ NodeElement textPathElement ]
        textPathElement = Element "{http://www.w3.org/2000/svg}textPath" (M.fromList [("href", "#circularPath"), ("startOffset", offsetString), ("method", "align"), ("side","right")]) [ NodeContent latticeText ]
        offset = 25 - div (clockDifference * 100) 24  
        offsetString = T.show offset <> "%"
        latticeText = T.intercalate "" $ take clockDifference $ repeat "XXXX"