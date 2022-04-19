module ForkIOStrat where


import Control.Concurrent ( forkIO, newMVar, takeMVar, putMVar, MVar, getNumCapabilities, threadDelay, readMVar )
import Lib (md5)
import GeneralFunctions(checkSymbCount, detectHash, generateAllPasswords, realDictionary)


forkIOHash minSymb maxSymb flag numCap hash dictionary  = do
                                                                    broot list hash flag strNumber lenList 
                                                                    answer <- readMVar flag
                                                                    if answer /= [] 
                                                                        then print answer 
                                                                        else if minSymb < maxSymb 
                                                                            then forkIOHash (minSymb+1) maxSymb flag numCap hash dictionary
                                                                            else print "Password is more than 6 symbols or no such symbols in dictionary"
                                                                                        where list = generateAllPasswords minSymb dictionary
                                                                                              lenList = length dictionary^minSymb
                                                                                              strNumber = (lenList`div` numCap) + 1
                                                       



broot :: [String] -> String -> MVar String -> Int -> Int -> IO ()
broot [] hash flag strNumb listLen = putStr "" 
broot xs hash flag strNumb listLen = 
       if strNumb >= listLen
           then helper hash xs flag 
           else do  
            forkIO $ helper hash (take strNumb xs) flag
            broot (drop strNumb xs) hash flag strNumb (listLen-strNumb)

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