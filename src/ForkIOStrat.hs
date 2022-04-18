module ForkIOStrat where


import Control.Concurrent ( forkIO, newMVar, takeMVar, putMVar, MVar, getNumCapabilities, threadDelay, readMVar )
import Control.Parallel
import Lib (md5)
import GeneralFunctions(checkSymbCount, detectHash, generateAllPasswords, realDictionary)


forkIOHash minSymb maxSymb flag numCap hash dictionary | minSymb < 5 = do
                                                                    broot (generateAllPasswords minSymb dictionary) hash flag numCap
                                                                    threadDelay 1000
                                                                    answer <- readMVar flag
                                                                    if answer /= [] then print answer else print "No answer!"
                                                       | otherwise = undefined 



broot :: [String] -> String -> MVar String -> Int -> IO ()
broot [] _ flag strNumb = do
    haveAnswer <- readMVar flag
    putStr "" 
broot xs hash flag strNumb = do
        forkIO $ helper hash (take strNumb xs) flag
        broot (drop strNumb xs) hash flag strNumb


printAnswer :: String -> MVar String -> IO ()
printAnswer str flag = do
    a <- takeMVar flag
    putMVar flag str
    putStr ""
helper::String -> [String] -> MVar String -> IO()
helper hash (origPass:xs) flag = if hash == md5 origPass
    then printAnswer origPass flag
    else helper hash xs flag
helper hash [] flag = putStr ""