{-# LANGUAGE OverloadedStrings #-}

module Svg.Lesions 
(latticeElement
, tearElement
, roundHoleElement
, cobbleStoneElement
, encirclingBandElement
, cryoLesionElement
, laserLesionElement
, detachmentElement
) 
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

tearElement :: Clock -> Eccentricity -> Element
tearElement clock ecc = rotateElement clock $ translateElement ecc $ Element "{http://www.w3.org/2000/svg}path" attr []
  where attr =  (M.fromList [ ("d", tearPath)
                            , ("fill", "red")
                            , ("stroke", "blue")
                            , ("stroke-width", "2")])

roundHoleElement :: Clock -> Eccentricity -> Element
roundHoleElement clock ecc = rotateElement clock $ translateElement ecc $ Element "{http://www.w3.org/2000/svg}circle" attr []
  where attr =  (M.fromList [ ("r", "5")
                            , ("cx", "200")                                                         
                            , ("cy", "200")
                            , ("fill", "red")
                            , ("stroke", "blue")
                            , ("stroke-width", "2")])

cobbleStoneElement :: Clock -> Eccentricity -> Element
cobbleStoneElement clock ecc = rotateElement clock $ translateElement ecc $ Element "{http://www.w3.org/2000/svg}rect" attr []
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

latticeElement :: Clock -> Eccentricity -> ClockAngle -> Element
latticeElement clock ecc clockDifference = 
  rotateElement clock $ Element
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

encirclingBandElement :: Element
encirclingBandElement = Element "{http://www.w3.org/2000/svg}circle" attr []
  where attr = (M.fromList [ ("cx", "200")
                           , ("cy", "200")
                           , ("r", "98")
                           , ("fill", "none")
                           , ("style", "stroke:brown;stroke-width:4px;")
                           ])


cryoLesionElement :: Clock -> Eccentricity -> Element
cryoLesionElement clock ecc = position $ Element "{http://www.w3.org/2000/svg}text" attr [NodeContent $ "C"]
  where position = (rotateElement clock) . (translateElement ecc) . (rotateElement (12 - clock))
        attr = M.fromList[ ("x", T.pack centerX)
                         , ("y", T.pack centerY)
                         , ("fill", "darkgreen")
                         , ("font-family", "sans-serif")
                         , ("font-weight", "bold")
                         , ("font-size", "20")
                         ]

laserLesionElement :: Clock -> Eccentricity -> Element
laserLesionElement clock ecc = position $ Element "{http://www.w3.org/2000/svg}text" attr [NodeContent $ "L"]
  where position = (rotateElement clock) . (translateElement ecc) . (rotateElement (12 - clock))
        attr = M.fromList[ ("x", T.pack centerX)
                         , ("y", T.pack centerY)
                         , ("fill", "darkgreen")
                         , ("font-family", "sans-serif")
                         , ("font-weight", "bold")
                         , ("font-size", "20")
                         ]

detachmentElement :: Element
detachmentElement = Element "{http://www.w3.org/2000/svg}ellipse" attr []
  where attr = M.fromList[ ("cx", "200")
                         , ("cy", "30")
                         , ("rx", "80")
                         , ("ry", "100")
                         , ("fill", "blue")
                         , ("fill-opacity", "0.4")
                         ]