{-# LANGUAGE BlockArguments #-}
module Main where

import GeneralFunctions(generateAllPasswords)
import ParListChunk(chunkHash)
import ParMapStrategy (mapHash)
import Control.Concurrent ( forkIO, newMVar, takeMVar, putMVar, MVar, getNumCapabilities )
import Lib (md5)
import ForkIOStrat (forkIOHash)

main :: IO ()
main = do
    putStrLn "Write your md-5 hash sum:"
    hash <- getLine 
    let dictionary = ['a'.. 'z'] ++ ['0'..'9'] 
    --let answer = chunkHash 1 5 hash dictionary
    --putStrLn answer
    haveAnswer <- newMVar []
    numCup <- getNumCapabilities 
    forkIOHash 1 2 haveAnswer numCup hash dictionary
    return ()

