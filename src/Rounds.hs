module Rounds(mainOperations)
where
import Input
import Data.Bits
import Data.Word
import Data.List (cycle)


funcF:: Word32 -> Word32 -> Word32 -> Word32
funcF x y z = (x .&. y) .|.  ((complement x) .&. z)

funcG:: Word32 -> Word32 -> Word32 -> Word32
funcG x y z = (x .&. z) .|.  ((complement x) .&. y)

funcH:: Word32 -> Word32 -> Word32 -> Word32
funcH x y z = (x `xor` y) `xor` z

funcI:: Word32 -> Word32 -> Word32 -> Word32
funcI x y z = y `xor` (x .|. (complement z))

defineConstants::  [Word32]
defineConstants = foldl (\acc x -> acc ++ [round $ (2^^32 * abs(sin x) - 0.5)] ) [] [1..64]

circleShiftLeft:: Word32 -> Int -> Word32
circleShiftLeft x k = (x `shiftL` k) .|. (x `shiftR` (32 - k))



mainOperations:: Int -> Word32 -> Word32 -> Word32 -> Word32 -> [Word32] -> [Word32]
mainOperations i a b c d ms | i <= 15 = mainOperations (i+1) d newB1 b c ms
                            | i <= 31 = mainOperations (i+1) d newB2 b c ms
                            | i <= 47 = mainOperations (i+1) d newB3 b c ms
                            | i <= 63 = mainOperations (i+1) d newB4 b c ms
                                        where f1 = funcF b c d 
                                              f2 = funcG d c b
                                              f3 = funcH b c d
                                              f4 = funcI b c d
                                              g1 = i
                                              g2 = (5*i + 1) `mod` 16
                                              g3 = (3*i + 5) `mod` 16
                                              g4 = (7*i) `mod` 16   
                                              newB1 = b + ((f1 + a + k + m1) `circleShiftLeft` (s!!i))
                                              newB2 = b + ((f2 + a + k + m2) `circleShiftLeft` (s!!i))
                                              newB3 = b + ((f3 + a + k + m3) `circleShiftLeft` (s!!i))
                                              newB4 = b + ((f4 + a + k + m4) `circleShiftLeft` (s!!i))
                                              m1 = ms !! g1
                                              m2 = ms !! g2
                                              m3 = ms !! g3
                                              m4 = ms !! g4
                                              k = defineConstants !! i
                                              s1 = take 16 $ cycle [7,12,17,22]
                                              s2 = take 16 $ cycle [5,9,14,20]
                                              s3 = take 16 $ cycle [4,11,16,23]
                                              s4 = take 16 $ cycle [6,10,15,21]
                                              s = s1 ++ s2 ++ s3 ++ s4

mainOperations 64 a b c d ms = [a + a0,b + b0,c + c0,d + d0]
                                where a0 = 1732584193
                                      b0 = 4023233417
                                      c0 = 2562383102   
                                      d0 = 271733878     


