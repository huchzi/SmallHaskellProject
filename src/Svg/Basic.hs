{-# LANGUAGE OverloadedStrings #-}

module Svg.Basic (Eye(..)
                 , ClockAngle
                 , Clock
                 , Eccentricity
                 , clockToDegree
                 , eccentricityToPixels
                 , polarToCartesian
                 , centerX
                 , centerY) 
    where

import Text.XML
import qualified Data.Map  as M
import qualified Data.Text as T

-- Constants
centerX :: String
centerX = "200"

centerY :: String
centerY = "200"

oraDistance :: Int
oraDistance = 142

data Eye = RightEye | LeftEye

-- Functions for transforming

type Clock = Int
type ClockAngle = Int
type Eccentricity = Int

clockToDegree :: Clock -> Int
clockToDegree clock = 30 * (clock - 3) + 90

polarToCartesian :: Int -> Eccentricity -> (Int, Int)
polarToCartesian angle ecc = (round x, round y)
  where x = radius * (cos ((fromIntegral angle) * pi / 180)) + (read centerX)
        y = radius * (sin ((fromIntegral angle) * pi / 180)) + (read centerY)
        radius = fromIntegral ecc

eccentricityToPixels :: Eccentricity -> Int
eccentricityToPixels ecc = div (ecc * oraDistance) 120