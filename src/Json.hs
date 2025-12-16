{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json (Fundus(..), eyeSide, tears, equatorial) where

import Svg
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Text.XML
import qualified Data.Map        as M


data Fundus = Fundus 
    { eyeSide :: String
    , tears :: [(String, Int, Int)]
    , equatorial :: [(String, Int, Int, Int)]
    } deriving (Show, Generic)

instance FromJSON Fundus