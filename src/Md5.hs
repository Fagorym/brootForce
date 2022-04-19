module Md5
    ( md5
    ) where

import Rounds
import Input
import Data.Char(ord)
import Data.Bits
import Data.Word(Word8, Word32)
import Numeric (showHex)


md5 :: String -> String
md5 input = printHash $ from32To8 $ getHash input


getHash::String -> [Word32]
getHash name =  mainOperations 0 a0 b0 c0 d0 (input name)
                        where a0 = 1732584193
                              b0 = 4023233417
                              c0 = 2562383102   
                              d0 = 271733878     

printHash:: [Word8] -> String
printHash [] = ""
printHash (x:xs) = (replicate (2 - (length symbol)) '0') ++ symbol ++ printHash xs
                        where symbol = showHex x  ""



from32To8::[Word32] -> [Word8]
from32To8 [] = []
from32To8 (x:xs) = (helper x 0)++(from32To8 xs)
                           where   helper:: Word32 -> Int -> [Word8]
                                   helper x 32 = []  
                                   helper x acc = (fromIntegral ((x `shiftR` acc) .&. 0xff)): (helper x (acc + 8))
                        