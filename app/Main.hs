module Main where

import ParListChunk(doAll)

main :: IO ()
main = do
    putStrLn "Write your md-5 hash sum:"
    hash <- getLine 
    let dictionary = ['a'.. 'z'] ++ ['0'..'9'] 
    let answer = doAll 1 5 hash dictionary
    putStrLn answer