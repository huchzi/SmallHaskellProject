{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Text.XML
import qualified Data.Map        as M

data Example = Example 
    { exampleType :: String,
      exampleList :: [String]
    } deriving (Show, Generic)

instance FromJSON Example

svgName :: Name
svgName = "{http://www.w3.org/2000/svg}svg"

exampleSvg :: Document
exampleSvg =
  Document
    (Prologue [] Nothing [])
    (Element svgName (M.fromList [("width", "100"), ("height", "100")]) [NodeElement node1, NodeElement node2])
    []
    where node1 = Element "{http://www.w3.org/2000/svg}circle" (M.fromList [("cx", "50"), ("cy", "50"), ("r", "40"), ("fill", "red")]) []
          node2 = Element "{http://www.w3.org/2000/svg}circle" (M.fromList [("cx", "50"), ("cy", "50"), ("r", "30"), ("fill", "green")]) []

main :: IO ()
main = do
    -- Beziehung zum Nutzer herstellen
    putStrLn "Wie heißt Du?"
    name <- getLine
    putStrLn $ "Hallo, " ++ name ++ "!"

    -- Eine JSON-Datei einlesen
    jsonData <- B.readFile "data/example.json"
    print jsonData

    -- Die JSON-Datei in den Datentyp Example parsen
    let example = decode jsonData :: Maybe Example
    case example of
        Just ex -> print ex
        Nothing -> putStrLn "JSON Parsing failed."

    -- Etwas mit diesen Daten machen
    case example of
        Just ex -> mapM_ print $ zip (map show [1..]) (exampleList ex)

    -- Ein SVG ausgeben
    Text.XML.writeFile def "test.svg" exampleSvg