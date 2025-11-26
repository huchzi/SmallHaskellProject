module Main (main) where

import Lib

main :: IO ()
main = do
    putStrLn "Wie heißt Du?"
    name <- getLine
    putStrLn $ "Hallo, " ++ name ++ "!"
