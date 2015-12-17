module FCM where

import qualified Data.List.Split as S
import Data.List
import System.Random
import Arguments

m = 2

run :: StdGen -> Arguments -> [[Double]] -> [[Double]]
run stdGen args objmatrix = let umatrix = uMatrixAtFirstStep stdGen args objmatrix
                            in nextStep args objmatrix umatrix

uMatrixAtFirstStep :: StdGen -> Arguments -> [[Double]] -> [[Double]]
uMatrixAtFirstStep stdGen args objmatrix = if random_matrix args
                                           then randGenAssign stdGen (clusters args) $ length objmatrix
                                           else genAssign args centers objmatrix
                                        where centers = getRandomCenters stdGen (clusters args) objmatrix

nextStep :: Arguments -> [[Double]] -> [[Double]] -> [[Double]]
nextStep args objmatrix umatrix = result
                                  where result = if norma < p then umatrixNew else nextStep args objmatrix umatrixNew
                                        p = precision args
                                        norma = matrixNorma umatrixNew umatrix
                                        umatrixNew = genAssign args centers objmatrix
                                        centers = genCenters args umatrix objmatrix


euclid :: [Double] -> [Double] -> Double
euclid = (sqrt .) . (sum .) . zipWith (\x v -> (x - v)**2)


hamming :: [Double] -> [Double] -> Double
hamming = (sum .) . zipWith (\x v -> abs (x - v))


distance :: [Char] -> [Double] -> [Double] -> Double
distance "E" = hamming
distance x = euclid


matrixNorma :: [[Double]] -> [[Double]] -> Double
matrixNorma kss1 kss = maximum $ zipWith ((abs .) . (-)) (concat kss1) (concat kss)


genCenters :: Arguments -> [[Double]] -> [[Double]]-> [[Double]]
genCenters args umatrix objmatrix = map byL (transpose umatrix)
    where byL us = map (byJ (pow us)) (transpose objmatrix)
          byJ p_us xs = sum (zipWith (*) p_us xs) / sum p_us
          pow = map (** m)


getRandomCenters :: StdGen -> Int -> [[Double]] -> [[Double]]
getRandomCenters stdGen c objmatrix = map (objmatrix !!) (take c $ randomRs (0, length objmatrix - 1) stdGen :: [Int])


genAssign :: Arguments -> [[Double]] -> [[Double]]-> [[Double]]
genAssign args vss = map byI
    where byI xs = map (byK xs) vss
          byK xs vs_k = if xs == vs_k then 1
                        else 1.0 / (sum . pow) (map (\vs_j -> d xs vs_k / d xs vs_j) vss)
          pow = map (**(2/(m - 1)))
          d = distance $ algorytm args


randGenAssign :: StdGen -> Int -> Int  -> [[Double]]
randGenAssign stdGen c n
  | n == 0 = []
  | otherwise =  map normalize randList
                  where randList = S.chunksOf c $ take (c * n) (randoms stdGen :: [Double])
                        normalize xs = map (/ sum xs) xs

