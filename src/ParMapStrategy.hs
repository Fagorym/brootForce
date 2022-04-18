module ParMapStrategy(mapHash, mapHashLong)
where

import GeneralFunctions
import Control.Parallel.Strategies ( parMap, rdeepseq, rpar )
import Lib



mapHash:: Int -> Int ->  Hash -> SymbolList -> Password
mapHash minSymb maxSymb hash dictionary | minSymb < 5 = maybe (checkSymbCount minSymb maxSymb (mapHash (minSymb + 1) maxSymb hash dictionary)) fst answerTuple
                                        | otherwise = mapHashLong minSymb maxSymb hash dictionary
                                                    where answerTuple = detectHash (calculateAllHashes $ generateAllPasswords minSymb realDictionary) hash


mapHashLong:: Int -> Int ->  Hash -> SymbolList -> Password
mapHashLong minSymb maxSymb hash [] = checkSymbCount minSymb maxSymb (mapHashLong (minSymb+1) maxSymb hash realDictionary)
mapHashLong minSymb maxSymb hash (x:dictionary) = maybe (checkSymbCount minSymb (maxSymb+1) (mapHashLong minSymb maxSymb hash dictionary)) fst answerTupleForLong
                                                                    where symb = x
                                                                          answerTupleForLong = detectHash (calculateAllHashes $ addSymb symb (generateAllPasswords 4 realDictionary)) hash
                                                                          addSymb:: Symbol -> [Password] -> [Password]
                                                                          addSymb symb list = parMap rpar (symb:) list


calculateAllHashes:: [Password]  -> [(Password,Hash)]
calculateAllHashes allPass = zip allPass hashPasses
                            where hashPasses = parMap rdeepseq md5 allPass