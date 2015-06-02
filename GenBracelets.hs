{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

import           Control.Monad              (forM_, when)
import           Control.Monad.ST
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Writer
-- import           Data.Array.Base            (unsafeRead, unsafeWrite)
import           Data.Array.ST
import qualified Data.IntMap.Strict         as IM
import           System.Environment         (getArgs)

type PreNecklace = [Int]
type Necklace = PreNecklace

-- genNecklaces is actually about 8x faster than genNecklacesST!

-- | @genNecklaces k n@ generates unique length-n necklaces over k
--   symbols.
genNecklaces :: Int -> Int -> [Necklace]
genNecklaces k n = execWriter (go 1 1 [0])
  where
    go :: Int -> Int -> PreNecklace -> Writer [Necklace] ()
    go t p pre
      | t > n     = candidate p pre
      | otherwise = do
          let a' = pre !! (p-1)
          go (t+1) p (a' : pre)
          forM_ [a'+1 .. k-1] $ \j ->
            go (t+1) t (j : pre)

    candidate :: Int -> PreNecklace -> Writer [Necklace] ()
    candidate p pre = when (n `mod` p == 0) $ tell [tail $ reverse pre]

-- Note: changing readArray, writeArray to unsafeRead, unsafeWrite
-- does not really affect running time

genNecklacesST :: Int -> Int -> [Necklace]
genNecklacesST k n = runST (execWriterT genNecklacesST')
  where
    genNecklacesST' :: forall s. WriterT [Necklace] (ST s) ()
    genNecklacesST' = do
      pre <- lift $ newArray (0,n) 0
      go 1 1 pre

    go :: forall s. Int -> Int -> STArray s Int Int -> WriterT [Necklace] (ST s) ()
    go t p pre
      | t > n     = candidate p pre
      | otherwise = do
          a' <- lift $ readArray pre (t-p)
          lift $ writeArray pre t a'
          go (t+1) p pre
          forM_ [a'+1 .. k-1] $ \j -> do
            lift $ writeArray pre t j
            go (t+1) t pre

    candidate :: forall s. Int -> STArray s Int Int -> WriterT [Necklace] (ST s) ()
    candidate p pre = when (n `mod` p == 0) $ do
      neck <- lift $ getElems pre
      tell [tail neck]

----------------------------------------------------------------------
-- A CAT algorithm for generating all k-ary bracelets of a given
-- length, taken from
--
-- Sawada, Joe. "Generating bracelets in constant amortized time."
-- SIAM Journal on Computing 31, no. 1 (2001): 259-268.
--
-- This isn't really what we want (we want to generate bracelets with
-- a given fixed number of elements of each sort -- for that see
-- http://www.cis.uoguelph.ca/~sawada/papers/fix-brace.pdf) but it's a
-- good warmup.
--
-- Actually this might really be O(n) instead of CAT since unlike
-- Sawada's paper we use indexing into linked lists instead of O(1)
-- indexing into an array.

type Bracelet = Necklace

-- A prenecklace, stored backwards, along with its length and its
-- first element cached for quick retrieval.
data Pre = Pre !Int (Maybe Int) PreNecklace
  deriving (Show)

emptyPre :: Pre
emptyPre = Pre 0 Nothing []

class Snocable p a where
  (|>) :: p -> a -> p

instance Snocable Pre Int where
  (Pre 0 _ []) |> a  = Pre 1 (Just a) [a]
  (Pre t a1 as) |> a = Pre (t+1) a1 (a:as)

-- 1-based indexing
class Indexable p where
  (!) :: p -> Int -> Int

instance Indexable Pre where
  _ ! 0 = 0
  (Pre _ (Just a1) _) ! 1 = a1
  (Pre t _ as) ! i = as !! (t-i)
    -- as stores  a_t .. a_1.
    -- a_1 is the last element, i.e. with index t-1.
    -- a_2 has index t-2.
    -- In general, a_i has index t-i.

getPre :: Pre -> PreNecklace
getPre (Pre _ _ as) = reverse as

checkRev :: Pre -> Int -> Int
checkRev (Pre t _ as) i
  | reverse chunk > chunk = -1
  | reverse chunk < chunk = 0
  | otherwise             = 1
  where
    chunk = take (t - 2*i) . drop i $ as
  -- need to check i+1 .. t-i  against its reverse,
  -- which has indices i .. t-i-1.

genBracelets :: Int -> Int -> [Bracelet]
genBracelets k n = execWriter (go 1 1 0 0 0 False emptyPre)
  where
    go :: Int -> Int -> Int -> Int -> Int -> Bool -> Pre
       -> Writer [Bracelet] ()
    go t p r u v rs pre
      | t > n = when (not rs' && (n `mod` p == 0)) $ tell [getPre pre]
                -- can generate aperiodic bracelets by changing
                -- (n `mod` p == 0)  to  (p == n)
      | otherwise = do
          let a' = pre ! (t-p)
              pre' = pre |> a'
              v' | a' == (pre' ! 1)  = v + 1
                 | otherwise = 0
              u' | u == t - 1 && pre' ! (t-1) == pre' ! 1 = u + 1
                 | otherwise                    = u
          when (t /= n || u' == n || pre' ! n /= pre' ! 1) $ do
            case (u' == v') of
              True ->
                case checkRev pre' u' of
                  0 -> go (t+1) p r u' v' rs' pre'
                  1 -> go (t+1) p t u' v' False pre'
                  _ -> return ()
              False -> go (t+1) p r u' v' rs' pre'
          let u'' | u' == t   = u' - 1
                  | otherwise = u'
          forM_ [a'+1 .. k-1] $ \j ->
            case t of
              1 -> go (t+1) t r 1 1 rs' (pre |> j)
              _ -> go (t+1) t r u'' 0 rs' (pre |> j)
      where
        rs' | (t-1 > (n-r) `div` 2 + r) && pre ! (t-1) > pre ! (n-t+2+r) = False
            | (t-1 > (n-r) `div` 2 + r) && pre ! (t-1) < pre ! (n-t+2+r) = True
            | otherwise = rs

----------------------------------------------------------------------
-- A CAT algorithm for generating all length-n bracelets with a given
-- fixed content, taken from
--
-- S. Karim, J. Sawada, Z. Alamgir, and S. M. Husnine. "Generating
-- Bracelets with Fixed Content".
-- http://www.cis.uoguelph.ca/~sawada/papers/fix-brace.pdf
----------------------------------------------------------------------

-- Run-length encodings.  Stored in *reverse* order for easy access to
-- the end.
data RLE a = RLE !Int [(a,Int)]

emptyRLE :: RLE a
emptyRLE = RLE 0 []

removeLastRLE :: RLE a -> RLE a
removeLastRLE (RLE _ []) = error "removeLastRLE on []"
removeLastRLE (RLE n ((a,v):rest))
  | v > 1     = RLE n ((a,v-1):rest)
  | otherwise = RLE (n-1) rest

instance Indexable (RLE Int) where
  (RLE _ []) ! _ = error "Bad index in (!) for RLE"
  (RLE n ((a,v):rest)) ! i
    | i <= v = a
    | otherwise = (RLE (n-1) rest) ! (i-v)

instance Eq a => Snocable (RLE a) a where
  (RLE _ []) |> a' = RLE 1 [(a',1)]
  (RLE n rle@((a,v):rest)) |> a'
    | a == a'   = RLE n ((a,v+1):rest)
    | otherwise = RLE (n+1) ((a',1):rle)

-- Prenecklaces along with a run-length encoding.
data Pre' = Pre' Pre (RLE Int)

emptyPre' :: Pre'
emptyPre' = Pre' emptyPre emptyRLE

instance Indexable Pre' where
  _ ! 0 = 0
  (Pre' (Pre len _ _) rle) ! i = rle ! (len - 1 - i)

instance Snocable Pre' Int where
  (Pre' p rle) |> a = Pre' (p |> a) (rle |> a)

genFixedBracelets :: IM.IntMap Int -> Int -> [Bracelet]
genFixedBracelets content n = execWriter (go 1 1 0 0 0 False content emptyPre')
  where
    go :: Int -> Int -> Int -> Int -> Int -> Bool -> IM.IntMap Int -> Pre'
       -> Writer [Bracelet] ()
    go t p r z b rs content pre = undefined

      where
        rs' | (t-1 > (n-r) `div` 2 + r) && pre ! (t-1) > pre ! (n-t+2+r) = False
            | (t-1 > (n-r) `div` 2 + r) && pre ! (t-1) < pre ! (n-t+2+r) = True
            | otherwise = rs

----------------------------------------------------------------------
----------------------------------------------------------------------

main :: IO ()
main = do
  [k, n] <- getArgs
  let bs = genBracelets (read k) (read n)
  print bs
  print (length bs)
