module ForkIOStrat where


import Control.Concurrent ( forkIO, newMVar, takeMVar, putMVar, MVar, getNumCapabilities, threadDelay, readMVar )
import Control.Parallel
import Lib (md5)
import GeneralFunctions(checkSymbCount, detectHash, generateAllPasswords, realDictionary)


forkIOHash minSymb maxSymb flag numCap hash dictionary | minSymb < 5 = do
                                                                    broot (generateAllPasswords minSymb dictionary) hash flag numCap
                                                                    threadDelay 100
                                                                    answer <- readMVar flag
                                                                    if answer /= [] 
                                                                        then print answer 
                                                                        else if minSymb < maxSymb 
                                                                            then forkIOHash (minSymb+1) maxSymb flag numCap hash dictionary
                                                                            else print "No answer" 
                                                       | otherwise = undefined 



broot :: [String] -> String -> MVar String -> Int -> IO ()
broot [] hash flag strNumb = do
    haveAnswer <- readMVar flag
    putStr "" 
broot xs hash flag strNumb = do
        forkIO $ helper hash (take strNumb xs) flag
        broot (drop strNumb xs) hash flag strNumb

helper::String -> [String] -> MVar String -> IO()
helper hash (origPass:xs) flag = do
    a <- readMVar flag
    if a /= [] then putStr "" else if hash == md5 origPass
    then getAnswer origPass flag
    else helper hash xs flag
helper hash [] flag = putStr ""

getAnswer :: String -> MVar String -> IO ()
getAnswer str flag = do
    a <- takeMVar flag
    putMVar flag str
    putStr ""