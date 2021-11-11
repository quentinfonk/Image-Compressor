module Lib where

import System.Exit
import Data.Char
import System.Exit

isArgs :: [String] -> Bool
isArgs [] = False
isArgs [[]] = False
isArgs _ = True

checkpresent :: [String] -> [Bool] -> Int -> Bool
checkpresent [] _ 3 = True
checkpresent [] _ _ = False
checkpresent (x:xs) (a:b:c:ab) d
               |  x == "-n" && not a = checkpresent xs [True, b, c, False] (d + 1)
               |  x == "-l" && not b = checkpresent xs [a, True, c, False] (d + 1)
               |  x == "-f" && not c = checkpresent xs [a, b, True, False] (d + 1)
               |  x == "-n" && a = False
               |  x == "-l" && b = False
               |  x == "-f" && c = False
               |  otherwise = checkpresent xs [a,b,c, False] d

putError :: IO Int
putError = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n\n\tN\tnumber of colors in the final image\n\tL\tconvergence limit\n\tF\tpath to the file containing the colors of the pixels" >> exitWith(ExitFailure 84)

errorHandleur :: [String] -> IO Int
errorHandleur arg | not (isArgs arg) = putError
                  | not (checkpresent arg [False, False, False, True] 0) = putError
                  | otherwise = return 0

getNbr :: [String] -> Int
getNbr [] = 0
getNbr (x:xs) = read x :: Int

carrer :: Float -> Float -> Float
carrer a b = (a*a) - 2 * (a*b) + (b*b)

printeee :: Float -> IO ()
printeee nb = putStrLn (show nb)


numToInt :: String -> Int
numToInt = foldl f 0 . map digitToInt
        where f x y = 10 * x + y

numToFloat :: String -> Float
numToFloat = fromIntegral . numToInt

divi :: Float -> Float -> Float
divi _ 0 = 0
divi a b = a / b

myLength :: [a] -> Int
myLength = foldr (\ x -> (+) 1) 0

recup :: String -> String -> String -> Int -> Int -> Int -> Float
recup (x:xs) y z nb1 nb2 nb3
    |    nb2 == nb1 = numToFloat y
    |    all isDigit [x] = recup xs (y ++ [x]) [x] nb1 nb2 nb3
    |    not (all isDigit [x]) && nb3 == 0 = recup xs "" [x] nb1 nb2 (nb3 + 1)
    |    not (all isDigit [x]) && all isDigit z && nb2 + 1 == nb1 = numToFloat y
    |    not (all isDigit [x]) && all isDigit z = recup xs "" [x] nb1 (nb2 + 1) nb3
    |    otherwise = recup xs "" [x] nb1 nb2 nb3

strToWordArray :: String -> String -> [String] -> [String]
strToWordArray (w:ws) a b
    |   w == '\n' && myLength ws == 0 = b ++ [a]
    |   w /= '\n' && myLength ws == 0 = b ++ [(a ++ [w])]
    |   w /= '\n' = strToWordArray ws (a ++ [w]) b
    |   w == '\n' = strToWordArray ws "" (b ++ [a])

reveve :: [String] -> String
reveve (x:xs) = x

nb_ligne :: [String] -> Int -> Int
nb_ligne [] a = a
nb_ligne (x:xs) y = nb_ligne xs (y + 1)

r_line :: [String] -> Int -> Int -> String
r_line (x:[]) _ _ = x
r_line (x:xs) y z | y == z = x
                  | otherwise = r_line xs (y + 1) z


gestNumberOfCluster :: [String] -> Int -> IO Int
gestNumberOfCluster str y | nb_ligne str 0 < y = putError
                          | otherwise = return 0
