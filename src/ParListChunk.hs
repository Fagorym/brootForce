module ParListChunk (
    chunkHash,
    generateAllPasswords) where

import Lib ( md5 )
import Control.Parallel.Strategies
    ( parListChunk, rdeepseq, using )
import Data.Char (chr)
import Data.Maybe(fromJust,isNothing)
import GeneralFunctions
    ( checkSymbCount,
      detectHash,
      generateAllPasswords,
      Hash,
      Password,
      Symbol,
      SymbolList,
      realDictionary )

chunkHash:: Int -> Int ->  Hash -> SymbolList -> Password
chunkHash minSymb maxSymb hash dictionary | minSymb < 5 = maybe (checkSymbCount minSymb maxSymb (chunkHash (minSymb + 1) maxSymb hash dictionary)) fst answerTuple
                                        | otherwise = chunkHashLong minSymb maxSymb hash dictionary
                                                    where answerTuple = detectHash (calculateAllHashes $ generateAllPasswords minSymb realDictionary) hash


chunkHashLong:: Int -> Int ->  Hash -> SymbolList -> Password
chunkHashLong minSymb maxSymb hash [] = checkSymbCount minSymb maxSymb (chunkHashLong (minSymb+1) maxSymb hash realDictionary)
chunkHashLong minSymb maxSymb hash (x:dictionary) = maybe (checkSymbCount minSymb (maxSymb+1) (chunkHashLong minSymb maxSymb hash dictionary)) fst answerTupleForLong
                                                                    where symb = x
                                                                          answerTupleForLong = detectHash (calculateAllHashes $ addSymb symb (generateAllPasswords 4 realDictionary)) hash
                                                                          addSymb:: Symbol -> [Password] -> [Password]
                                                                          addSymb symb list = map (symb:) list `using` parListChunk 500 rdeepseq


calculateAllHashes:: [Password]  -> [(Password,Hash)]
calculateAllHashes allPass = zip allPass hashPasses
                            where hashPasses = map md5 allPass `using` parListChunk 500 rdeepseq
