module Lib
    ( md5
    ) where

import Rounds ( mainOperations )
import Input ( input )
import Data.Char(ord,intToDigit)
import Data.Bits ( Bits((.&.), shiftR) )
import Data.Word ( Word8, Word32 )
import Data.List (cycle)
import Numeric (showHex)


md5 :: String -> String
md5 name = printResult $ transformToWord8 $ getHash name


getHash::String -> [Word32]
getHash name =  mainOperations 0 a0 b0 c0 d0 (input name)
                        where a0 = 1732584193
                              b0 = 4023233417
                              c0 = 2562383102
                              d0 = 271733878

printResult:: [Word8] -> String
printResult [] = ""
printResult (x:xs) = replicate (2 - length symbol) '0' ++ symbol ++ printResult xs
                        where symbol = showHex x  ""



transformToWord8::[Word32] -> [Word8]
transformToWord8 [] = []
transformToWord8 (x:xs) = helper x 0 ++ transformToWord8 xs
                           where   helper:: Word32 -> Int -> [Word8]
                                   helper x 32 = []
                                   helper x acc = fromIntegral ((x `shiftR` acc) .&. 0xff): helper x (acc + 8)
                        