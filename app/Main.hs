{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Svg
import Svg.Lesions
import Json
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Text.XML
import qualified Data.Map        as M

main :: IO ()
main = do
    jsonData <- B.readFile "data/drawing1.json"
    let drawing = decode jsonData :: Maybe Fundus
    print drawing
    case drawing of
        Just f ->  Text.XML.writeFile def "data/parsedFundus.svg" $ jsonToSvg f
        Nothing -> putStrLn "JSON Parsing failed."

    -- Ein SVG ausgeben
    Text.XML.writeFile def "data/fundus.svg" $ fundusDrawing LeftEye  [ detachmentGroup [detachmentElement 3, detachmentElement 4]
                                                                      , encirclingBandElement
                                                                      , latticeElement 3 (eccentricity Equatorial) 2 
                                                                      , tearElement 3 $ eccentricity Equatorial
                                                                      , roundHoleElement 11 $ eccentricity Anterior
                                                                      , cobbleStoneElement 6 $ eccentricity PreEquatorial
                                                                      , laserLesionElement 7 $ eccentricity PreEquatorial
                                                                      ]
    
