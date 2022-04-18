module GeneralFunctions where

import Control.Monad ( replicateM )
import Data.List(find)

realDictionary = ['a'.. 'z'] ++ ['0'..'9']
type Hash = String
type Password = String
type SymbolList = [Symbol]
type Symbol = Char

generateAllPasswords:: Int -> SymbolList -> [Password]
generateAllPasswords = replicateM


detectHash:: [(Password,Hash)] -> Hash -> Maybe (Password,Hash)
detectHash tuples hashtofind =  find (\x -> snd x == hashtofind) tuples


checkSymbCount:: Int -> Int -> Password -> String
checkSymbCount min max currentFunction = if min >= max then "Password is longer than " ++ show max ++ " symbols"
                                                else currentFunction
