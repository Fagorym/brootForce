module Main where

import Control.Concurrent
import Control.Parallel.Strategies
import Control.DeepSeq
import System.Environment
import Control.Monad
import Data.Char (chr)
import Data.List(find)
import Data.Maybe(fromJust,isNothing)
import Lib

type Hash = String
type Password = String
type SymbolList = [Symbol]
type Symbol = Char
realDictionary = ['a'.. 'z'] ++ ['0'..'9'] 

main :: IO ()
main = do
    putStrLn "Write your md-5 hash sum:"
    hash <- getLine 
    let dictionary = ['a'.. 'z'] ++ ['0'..'9'] 
    let answer = doAll 1 5 hash dictionary
    putStrLn answer

doAll:: Int -> Int ->  Hash -> SymbolList -> Password
doAll minSymb maxSymb hash dictionary | minSymb < 5 =  if isNothing answerTuple 
                                                    then checkSymbCount minSymb maxSymb (doAll (minSymb + 1) maxSymb hash dictionary)
                                                    else  fst $ fromJust $ answerTuple
                                      | otherwise = doAllForLongPasswords minSymb maxSymb hash dictionary
                                                                                                        where answerTuple = detectHash (calculateAllHashes $ generateAllPasswords minSymb realDictionary) hash


doAllForLongPasswords:: Int -> Int ->  Hash -> SymbolList -> Password
doAllForLongPasswords minSymb maxSymb hash [] = checkSymbCount minSymb maxSymb (doAllForLongPasswords (minSymb+1) maxSymb hash realDictionary)
doAllForLongPasswords minSymb maxSymb hash (x:dictionary) = if isNothing answerTupleForLong 
                                                    then checkSymbCount minSymb (maxSymb+1) (doAllForLongPasswords minSymb maxSymb hash dictionary)
                                                    else fst $ fromJust $ answerTupleForLong
                                                                                                        where symb = x
                                                                                                              answerTupleForLong = detectHash (calculateAllHashes $ addSymb symb (generateAllPasswords 4 realDictionary)) hash
                                                                                                              addSymb:: Symbol -> [Password] -> [Password]
                                                                                                              addSymb symb list = map (\y -> symb:y) list `using` parListChunk 1000 rdeepseq

checkSymbCount:: Int -> Int -> Password -> String
checkSymbCount min max currentFunction = if min >= max then "Password is longer than " ++ show max ++ " symbols"
                                                else currentFunction

generateAllPasswords:: Int -> SymbolList -> [Password] 
generateAllPasswords pow symb = replicateM pow symb


calculateAllHashes:: [Password]  -> [(Password,Hash)]
calculateAllHashes allPass = zip allPass hashPasses
                            where hashPasses = map md5 allPass `using` parListChunk 1000 rdeepseq



detectHash:: [(Password,Hash)] -> Hash -> Maybe (Password,Hash)
detectHash tuples hashtofind =  find (\x -> (snd x) == hashtofind) tuples

