-- Cycle index series for species of bracelets

{-# LANGUAGE NoImplicitPrelude #-}

module BraceletCI where

import           Control.Monad                         (forM_)
import           Data.List                             (sort)
import qualified Data.Map                              as M
import           Math.Combinatorics.Species
import           Math.Combinatorics.Species.CycleIndex
import           Math.Combinatorics.Species.Types
import qualified MathObj.FactoredRational              as FQ
import qualified MathObj.Monomial                      as Monomial
import qualified MathObj.MultiVarPolynomial            as MVP
import qualified MathObj.PowerSeries                   as PS
import           System.Environment                    (getArgs)
import           Text.Printf

import           NumericPrelude

braceletCI :: CycleIndex
braceletCI = ciFromMonomials . concatMap braceletMonomials $ [1..]

braceletMonomials :: Integer -> [Monomial.T Rational]
braceletMonomials 1 = [ Monomial.Cons 1 (M.singleton 1 1)]
braceletMonomials 2 = [ Monomial.Cons (1%2) (M.singleton 2 1)
                      , Monomial.Cons (1%2) (M.singleton 1 2)
                      ]
braceletMonomials n = sort $ flips : map rotations ds
  where n' = fromIntegral n
        ds = sort . FQ.divisors $ n'
        rotations k
          = Monomial.Cons
              ((FQ.eulerPhi k + (if kI == 2 then n `div` kI else 0)) % (2*n))
              (M.singleton kI (n `div` kI))
          where
            kI = toInteger k
        flips
          | odd n     = Monomial.Cons (1 % 2) (M.fromList [(1,1), (2, n `div` 2)])
          | otherwise = Monomial.Cons (1 % 4) (M.fromList [(1,2), (2, n `div` 2 - 1)])

numOPs :: Integer -> Integer
numOPs 1 = 0
numOPs n = PS.coeffs ps !! (2*fromIntegral n)
  where
    (GF ps) = zToGF $ braceletCI >< (set `ofSizeExactly` (n+2) * set `ofSizeExactly` (n-2))

main = do
  [bound] <- getArgs
  forM_ [1..read bound] $ \n ->
    printf "%2d %d\n" (2*n) (numOPs n)

{-

hippasus :: ~/research/ortho-polys » time ./BraceletCI
 2 0
 4 1
 6 1
 8 4
10 8
12 29
14 79
16 280
18 912
20 3260
22 11410
24 41272
26 148976
28 544802
30 1997499
32 7372080
34 27299360
36 101520714
38 378721134
40 1417339352
42 5318837680
44 20012141478
^C
./BraceletCI  205.87s user 1.18s system 100% cpu 3:26.95 total

-}
