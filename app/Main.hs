module Main where

import Lib
import Data.Char
import System.Exit
import System.Environment
import Data.Bits
import System.Random

data Info = Inf {
nb_colors :: Int,
limit :: Float,
path :: String
} deriving Show

data RgInfo = Rgb {
    x :: Float,
    y :: Float,
    r :: Float,
    g :: Float,
    b :: Float
} deriving Show

data CentrInfo = Centr {
     rc :: Float,
     gc :: Float,
     bc :: Float,
     tabpix :: [RgInfo]
}

recupArg :: [String] -> Info -> Info
recupArg [] inf = inf
recupArg [_] inf = inf {nb_colors = -1, limit = -1, path = "Error"}
recupArg (x:y:xs) inf
    |   x == "-n" && all isDigit y = recupArg xs (inf {nb_colors = getNbr [y]})
    |   x == "-l" = recupArg xs (inf {limit = read y :: Float})
    |   x == "-f" && not (all isDigit y) = recupArg xs (inf {path = y})
    |   otherwise = inf {nb_colors = -1, limit = -1, path = "Error"}

calcul :: RgInfo -> CentrInfo -> Float
calcul inf centr = sqrt (carrer (rc centr) (r inf) + carrer (gc centr) (g inf) + carrer (bc centr) (b inf))

add :: RgInfo -> [RgInfo] -> Bool -> Bool -> [RgInfo]
add _ [] True True = []
add rgb [] _ False = rgb : add rgb [] True True
add rgb (rg:rgbb) _ False = rg : add rgb rgbb False False

couc :: RgInfo
couc = Rgb {x = 26, y = 1, r = 56, g = 25, b = 89}

hello :: Int -> [RgInfo]
hello 0 = []
hello a = couc : hello (a-1)

rreemm :: CentrInfo
rreemm = Centr{rc = 89, gc = 89, bc = 89, tabpix = hello 1}

test :: Int ->  [CentrInfo]
test 0 = []
test a = rreemm : test (a-1)

remPixe :: RgInfo -> [CentrInfo] -> Int -> Bool -> [CentrInfo]
remPixe _ [] _ True = []
remPixe rgb (centr:centrInfo) nb True = centr : remPixe rgb centrInfo (nb-1) True
remPixe rgb (centr:centrInfo) nb False | nb > 1 = centr : remPixe rgb centrInfo (nb-1) False
                                    | otherwise = centr {tabpix = tabpix centr ++ [rgb]} : remPixe rgb centrInfo nb True

calc :: RgInfo -> [CentrInfo] -> Float -> Int -> Int -> [CentrInfo] -> [CentrInfo]
calc rgb [] _ b _ sauv = remPixe rgb sauv (b+1) False
calc rgb (centr:centrinfo) a b c sauv | calcul rgb centr <= a = calc rgb centrinfo (calcul rgb centr) c (c+1) sauv -- blem ici
                                      | otherwise = calc rgb centrinfo a b (c+1) sauv

algo :: [RgInfo] -> [CentrInfo] -> [CentrInfo]
algo [] centr = centr
algo (pix:pixels) centr = algo pixels (calc pix centr 1000 0 0 centr)

retttt :: RgInfo -> Float
retttt = g

looktab :: [RgInfo] -> Int -> IO ()
looktab [] a = putStrLn("FIN")
looktab (pix:pixels) a | (a == 0) = printeee (r pix)
                       | otherwise = looktab pixels (a-1)

printa :: [CentrInfo] -> IO ()
printa (centr:centrinfo) = looktab (tabpix centr) 0

printall :: [CentrInfo] -> Int -> IO ()
printall [] a = putStrLn "Fin"
printall (x:xs) a = print (tabpix x) >> printall xs (a+1)

recupInt :: Float -> Int
recupInt a = getNbr [show a]

azerf :: String -> String
azerf (x:xs) | x == '.' = ""
             | otherwise = x : azerf xs

pixPrint :: RgInfo -> Int -> String
pixPrint rgb 0 = "(" ++ pixPrint rgb 1
pixPrint rgb 1 = azerf (show (x rgb)) ++ pixPrint rgb 2
pixPrint rgb 2 = "," ++ pixPrint rgb 3
pixPrint rgb 3 = azerf (show (y rgb)) ++ pixPrint rgb 4
pixPrint rgb 4 = ") (" ++ pixPrint rgb 5
pixPrint rgb 5 = azerf (show (r rgb)) ++ pixPrint rgb 6
pixPrint rgb 6 = "," ++ pixPrint rgb 7
pixPrint rgb 7 = azerf (show (g rgb)) ++ pixPrint rgb 8
pixPrint rgb 8 = "," ++ pixPrint rgb 9
pixPrint rgb 9 = azerf (show (b rgb)) ++ pixPrint rgb 10
pixPrint rgb 10 = ")\n"

restTab :: [RgInfo] -> String
restTab [] = []
restTab (pix:pixels) = pixPrint pix 0 ++ restTab pixels

colorsPrint :: CentrInfo -> Int -> String
colorsPrint centr 0 = "(" ++ colorsPrint centr 1
colorsPrint centr 1 = azerf (show (rc centr)) ++ colorsPrint centr 2
colorsPrint centr 2 = "," ++ colorsPrint centr 3
colorsPrint centr 3 = azerf (show (gc centr)) ++ colorsPrint centr 4
colorsPrint centr 4 = "," ++ colorsPrint centr 5
colorsPrint centr 5 = azerf (show (bc centr)) ++ colorsPrint centr 6
colorsPrint centr 6 = ")\n-\n" ++ restTab (tabpix centr)

goodprint :: [CentrInfo] -> Int -> IO ()
goodprint [] _ = exitWith ExitSuccess
goodprint centr 0 = putStrLn "--" >> goodprint centr 1
goodprint (centr:centrinfo) 1 = putStr (colorsPrint centr 0) >> goodprint centrinfo 0

count :: [RgInfo] -> Float -> Float
count [] a = a
count (x:xs) a = count xs (a+1)

moye :: [RgInfo] -> Float -> Float -> Float -> Float -> Bool -> CentrInfo
moye [] a nb be c False = moye [] a nb be c True
moye [] a nb be c _ = Centr {rc = divi a nb, gc = divi be nb, bc = divi c nb, tabpix = []}
moye (pix:pixels) a nb be c _ = moye pixels (a + r pix) nb (be + g pix) (c + b pix) False

converg :: Info -> CentrInfo -> Bool
converg inf centr | calcul (Rgb {x = 0, y = 0, r = rc centr, g = gc centr, b = bc centr}) (moye (tabpix centr) 0 (count (tabpix centr) 0) 0 0 False) < limit inf = True
                  | otherwise = False

verifConver :: Info -> [CentrInfo] -> Bool
verifConver inf [] = True
verifConver inf (centr:centrinfo) | converg inf centr = verifConver inf centrinfo
                                  | otherwise = False

newCentra :: CentrInfo -> CentrInfo
newCentra centr = moye (tabpix centr) 0 (count (tabpix centr) 0) 0 0 False

newCentrinfo :: [CentrInfo] -> [CentrInfo]
newCentrinfo = map newCentra

imageCompressor :: Info -> [RgInfo] -> [CentrInfo] -> Int -> IO ()
imageCompressor inf rgb centr a | not (verifConver inf (algo rgb centr)) = imageCompressor inf rgb (newCentrinfo (algo rgb centr)) 1
                                | otherwise = goodprint (algo rgb centr) 0

cherckArg :: Info -> IO Int
cherckArg inf | nb_colors inf == -1 && limit inf == -1 && path inf == "Error" = exitWith(ExitFailure 84)
              | otherwise = return 0

fileParse :: [String] -> [RgInfo] -> [RgInfo]
fileParse [] pixels = pixels
fileParse (x:xs) (pix:next) = fileParse xs (pix {x = 26, y = 1, r = 56, g = 25, b = 89}:next)
fileParse _ [] = []

takinfo :: String -> RgInfo
takinfo str = Rgb {x = recup str "" "" 1 0 0, y = recup str "" "" 2 0 0, r = recup str "" "" 3 0 0, g = recup str "" "" 4 0 0, b = recup str "" "" 5 0 0}

remp :: [String] -> [RgInfo]
remp = map takinfo

supp :: [String] -> Int -> [String]
supp [] _ = []
supp (x:xs) 0 = supp xs (-1)
supp (x:xs) nb = x : supp xs (nb-1)

ranna :: [String] -> Int -> IO CentrInfo
ranna str sta = do
   return Centr{rc = recup (r_line str 0 sta) "" "" 3 0 0, gc = recup (r_line str 0 sta) "" "" 4 0 0, bc = recup (r_line str 0 sta) "" "" 5 0 0, tabpix = []}

randoa :: Int -> [String] -> (a, b, c) -> IO [CentrInfo]
randoa 0 _ (a, b, c) = return []
randoa nb str (a, b, c) = do
    sta <- randomRIO (0, nb_ligne str (-1) :: Int)
    stp <- ranna str sta
    aze <- randoa (nb-1) (supp str sta) (recup (r_line str 0 sta) "" "" 3 0 0, recup (r_line str 0 sta) "" "" 4 0 0, recup (r_line str 0 sta) "" "" 5 0 0)
    return $ stp : aze

rettt :: RgInfo -> Float
rettt = x

veeerfi :: [RgInfo] -> Float
veeerfi (aa:rgb) = rettt aa

-- reveve :: [String] -> String
-- reveve (x:xs) = x

-- gestNumberOfCluster :: [String] -> Int -> IO Int
-- gestNumberOfCluster str y | nb_ligne str 0 < y = putError
--                           | otherwise = return 0

verif_line :: String -> [String] -> Bool
verif_line _ [] = True
verif_line x (y:ys) | (recup x "" "" 3 0 0) == (recup y "" "" 3 0 0) && (recup x "" "" 4 0 0) == (recup y "" "" 4 0 0) && (recup x "" "" 5 0 0) == (recup y "" "" 5 0 0) = False
                    | otherwise = verif_line x ys

gestNumberOfColors :: [String] -> [String] -> [String]
gestNumberOfColors [] y = y
gestNumberOfColors (x:xs) y | (verif_line x y) == True = gestNumberOfColors xs (y ++ [x])
                            | otherwise = y

recupinfotest :: [RgInfo] -> Int -> CentrInfo -> Float
recupinfotest rgb 0 centr = recupinfotest rgb 1 (newCentra centr)
recupinfotest rgb 1 centr = rc centr

main :: IO ()
main = do
    arg <- getArgs
    errorHandleur arg
    let inf = recupArg arg (Inf {nb_colors = 0, limit = 0, path = ""})
    cherckArg inf
    str <- readFile (path inf)
    let tab = strToWordArray str "" []
    let unique_colors = gestNumberOfColors tab []
    gestNumberOfCluster tab (nb_colors inf)
    let rgb = remp tab
    centr <- randoa (nb_colors inf) tab (0, 0, 0)
    imageCompressor inf rgb centr 0
    exitSuccess;
