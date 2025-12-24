{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


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

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Text (Text)
import Data.Text.Lazy (toStrict)

type API = "trigger" :> Post '[PlainText] Text

data Result = Result { resultText :: Text }
  deriving (Show)

server :: Server API
server = postTrigger
  where
    postTrigger :: Handler Text
    postTrigger = return resultValue

app :: Application
app = serve (Proxy :: Proxy API) server

resultValue :: Text
resultValue =  
  toStrict $ Text.XML.renderText 
              def 
              (fundusDrawing LeftEye  [ detachmentGroup [detachmentElement 3, detachmentElement 4]
                                      , encirclingBandElement
                                      , latticeElement 3 (eccentricity Equatorial) 2 
                                      , tearElement 3 $ eccentricity PreEquatorial
                                      , roundHoleElement 11 $ eccentricity Anterior
                                      , cobbleStoneElement 6 $ eccentricity PreEquatorial
                                      , laserLesionElement 7 $ eccentricity PreEquatorial
                                      ])

main :: IO ()
main = do
    jsonData <- B.readFile "data/drawing1.json"
    let drawing = decode jsonData :: Maybe Fundus
    print drawing
    case drawing of
        Just f ->  Text.XML.writeFile def "data/parsedFundus.svg" $ jsonToSvg f
        Nothing -> putStrLn "JSON Parsing failed."

    run 8080 app
    
