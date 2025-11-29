{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Svg
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

main :: IO ()
main = do

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
        Nothing -> putStrLn "JSON Parsin failed."

    -- Ein SVG ausgeben
    Text.XML.writeFile def "data/fundus.svg" fundusDrawing