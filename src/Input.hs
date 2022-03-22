module Input
(input
) where


import Data.Char (ord)
import Data.Word
import Data.Bits (shiftL)






stringToWord8:: String -> [Word8]
stringToWord8 xs =  foldl(\acc x -> fromIntegral (ord x): acc) [] xs -- Не реверснутая строка, коды элементов в порядке от последнего к первому


firstListAdditional:: [Word8] -> [Word8] -- Добавляем константу(10000000) и некоторое кол-во нулей
firstListAdditional xs = additional ++ (const:xs)
                                    where const = 128::Word8
                                          dif = 55 - length xs
                                          additional = (replicate dif (fromIntegral 0))::[Word8]

lengthAdditional:: [Word8] -> String -> [Word8] -- Добавляем длину слова и дополнительные нули
lengthAdditional xs name = (replicate 7 0) ++ ((fromIntegral(length name) * 8): xs)

to32Unsigned:: [Word8] -> [Word32] -- Преобразуем список 8 битных слов в список 32 битных слов (перед этим нужно их реверснуть!)
to32Unsigned [] = []
to32Unsigned xs = (transform (take 4 xs)): (to32Unsigned (drop 4 xs)) 


transform:: [Word8] -> Word32 -- преобразуем одно слово!
transform xs = helper xs 24
                           where   helper:: [Word8] -> Int -> Word32
                                   helper (x:xs) acc = ((fromIntegral x) `shiftL` (24 - acc)) + (helper xs (acc-8))
                                   helper [] _ = 0


input:: String -> [Word32]  -- Композиция всех функций
input name = to32Unsigned $ reverse $  lengthAdditional (firstListAdditional (stringToWord8 name)) name