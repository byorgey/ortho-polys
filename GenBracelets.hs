{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

import           Debug.Trace
import           Test.QuickCheck

import           Control.Arrow              (first, (&&&))
import           Control.Monad              (forM_, when)
import           Control.Monad.ST
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Writer
import           Data.List                  (group, sort)
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
checkRev pre@(Pre t _ _) i
  | reverse chunk < chunk = -1
  | reverse chunk > chunk = 0
  | otherwise             = 1
  where
    chunk = slicePre (i+1) (t-i) pre
  -- need to check i+1 .. t-i  against its reverse,
  -- which has indices i .. t-i-1.

-- slicePre i j pre returns  a_i .. a_j.
slicePre :: Int -> Int -> Pre -> [Int]
slicePre i j (Pre t _ as) = reverse . take (j - i + 1) . drop (t - j) $ as
  -- a_i, a_j have indices t-i, t-j respectively.  So first drop t-j.
  -- Then take t-i - t-j + 1 = j - i + 1.

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

-- First, a simple, slowish version.

simpleBFC :: Int -> Int -> [(Int,Int)] -> [Bracelet]
simpleBFC k n content = execWriter (go 1 1 0 content emptyPre)
  where
    go :: Int -> Int -> Int -> [(Int,Int)] -> Pre -> Writer [Bracelet] ()
    go t p r con pre@(Pre _ _ as)
      | t > n = itrace t ('!',t,p,r,con,pre)
              $ when (take (n - r) as >= reverse (take (n-r) as) && n `mod` p == 0)
              $ tell [getPre pre]
      | otherwise = itrace t ('.',t,p,r,con,pre) $ do
          let a' = pre ! (t-p)
          forM_ [a' .. (k-1)] $ \j -> itrace t j $ do
            let (con', nj') = decrease j con
                pre' = pre |> j
                c = checkRev2 t pre'
                p' | j /= a'   = t
                   | otherwise = p
            itrace t c $ do
              when (c == EQ && nj' >= 0) $ go (t+1) p' t con' pre'
              when (c == GT && nj' >= 0) $ go (t+1) p' r con' pre'

    -- itrace t x v = trace (concat (replicate t "  ") ++ show x) v
    itrace _ _ v = v

    decrease :: Int -> [(Int,Int)] -> ([(Int,Int)], Int)
    decrease _ [] = ([], -1)
    decrease j ((m,cnt):rest)
      | j == m = ( (if cnt == 1 then rest else (m,cnt-1):rest) , cnt-1)
      | otherwise = first ((m,cnt):) (decrease j rest)

    checkRev2 t pre = compare at1 a1t
      where
        a1t = slicePre 1 t pre
        at1 = reverse a1t

--------------------------------------------------
-- Some tools.

-- Run-length encodings.  Stored in *reverse* order for easy access to
-- the end.
data RLE a = RLE !Int !Int [(a,Int)]
  deriving (Show)
  -- First Int is the total length of the decoded list.
  -- Second Int is the number of blocks.

emptyRLE :: RLE a
emptyRLE = RLE 0 0 []

removeLastRLE :: RLE a -> RLE a
removeLastRLE (RLE _ _ []) = error "removeLastRLE on []"
removeLastRLE (RLE n b ((a,v):rest))
  | v > 1     = RLE (n-1) b ((a,v-1):rest)
  | otherwise = RLE (n-1) (b-1) rest

-- Indexing starts at 1 from the end of the list.
getBlockRLE :: RLE a -> Int -> (a,Int)
getBlockRLE (RLE n b blocks) ix
  | 1 <= ix && ix <= b = blocks !! (b - ix)
  | otherwise = error $ "Bad index (" ++ show ix ++ ") in getBlock for RLE with " ++ show b ++ "blocks."

compareRLE :: Ord a => [(a,Int)] -> [(a,Int)] -> Ordering
compareRLE [] [] = EQ
compareRLE [] _  = LT
compareRLE _ []  = GT
compareRLE ((a1,n1):rle1) ((a2,n2):rle2)
  | (a1,n1) == (a2,n2) = compareRLE rle1 rle2
  | a1 < a2 = LT
  | a1 > a2 = GT
  | (n1 < n2 && (null rle1 || fst (head rle1) < a2)) || (n1 > n2 && not (null rle2) && a1 < fst (head rle2)) = LT
  | otherwise = GT

runLengthEncode :: Eq a => [a] -> [(a,Int)]
runLengthEncode = map (head &&& length) . group

prop_compareRLE :: [Int] -> [Int] -> Bool
prop_compareRLE xs ys = compare xs ys == compareRLE (runLengthEncode xs) (runLengthEncode ys)

instance Indexable (RLE Int) where
  (RLE _ _ []) ! _ = error "Bad index in (!) for RLE"
  (RLE n b ((a,v):rest)) ! i
    | i <= v = a
    | otherwise = (RLE (n-v) (b-1) rest) ! (i-v)

instance Eq a => Snocable (RLE a) a where
  (RLE _ _ []) |> a' = RLE 1 1 [(a',1)]
  (RLE n b rle@((a,v):rest)) |> a'
    | a == a'   = RLE (n+1) b     ((a,v+1):rest)
    | otherwise = RLE (n+1) (b+1) ((a',1):rle)

-- Prenecklaces along with a run-length encoding.
data Pre' = Pre' Pre (RLE Int)
  deriving Show

emptyPre' :: Pre'
emptyPre' = Pre' emptyPre emptyRLE

getPre' :: Pre' -> PreNecklace
getPre' (Pre' pre _) = getPre pre

getBlock :: Pre' -> Int -> (Int,Int)
getBlock (Pre' _ rle) ix = getBlockRLE rle ix

instance Indexable Pre' where
  _ ! 0 = 0
  (Pre' (Pre len _ _) rle) ! i = rle ! (len - i + 1)

instance Snocable Pre' Int where
  (Pre' p rle) |> a = Pre' (p |> a) (rle |> a)

----------------------------------------------------------------------
-- An optimized version, incrementally optimized from the simple
-- version, making sure at each step that it is still correct and also
-- faster.

genFixedBracelets :: Int -> [(Int,Int)] -> [Bracelet]
genFixedBracelets n [(0,k)] | k >= n = [replicate k 0]
                            | otherwise = []
genFixedBracelets n content = execWriter (go 1 1 0 (IM.fromList content) emptyPre')
  where
    go :: Int -> Int -> Int -> IM.IntMap Int -> Pre' -> Writer [Bracelet] ()
    go _ _ _ con _ | IM.keys con == [0] = return ()
    go t p r con pre@(Pre' (Pre _ _ as) _)
      | t > n = itrace t ('!',t,p,r,con,pre)
              $ when (take (n - r) as >= reverse (take (n-r) as) && n `mod` p == 0)
              $ tell [getPre' pre]
      | otherwise = itrace t ('.',t,p,r,con,pre) $ do
          let a' = pre ! (t-p)
          forM_ (dropWhile (< a') $ IM.keys con) $ \j -> itrace t j $ do
            let con' = decrease j con
                pre' = pre |> j
                c = checkRev2 t pre'
                p' | j /= a'   = t
                   | otherwise = p
            itrace t c $ do
              when (c == EQ) $ go (t+1) p' t con' pre'
              when (c == GT) $ go (t+1) p' r con' pre'

    -- itrace t x v = trace (concat (replicate t "  ") ++ show x) v
    itrace _ _ v = v

    decrease :: Int -> IM.IntMap Int -> IM.IntMap Int
    decrease j con
      | IM.null con = con
      | otherwise   = IM.alter q j con
      where
        q (Just 1)   = Nothing
        q (Just cnt) = Just (cnt-1)
        q _          = Nothing

    checkRev2 _ (Pre' _ (RLE _ _ rle)) = compareRLE rle (reverse rle)

    -- checkRev2 t pre = compare at1 a1t
    --   where
    --     a1t = slicePre 1 t pre
    --     at1 = reverse a1t

prop_genFixed :: Positive Int -> [Int] -> Property
prop_genFixed (Positive n) con = not (null con) ==> genFixedBracelets n con' == simpleBFC k n con'
  where
    con' = runLengthEncode . sort $ con
    k    = succ . maximum . map fst $ con'

----------------------------------------------------------------------
-- An aborted attempt to transcribe the C algorithm directly from the
-- paper.

-- genFixedBracelets :: Int -> Int -> IM.IntMap -> [Bracelet]
-- genFixedBracelets k n content = execWriter (go 1 1 0 0 0 False content emptyPre')
--   where
--     go :: Int -> Int -> Int -> Int -> Int -> Bool -> IM.IntMap Int -> Pre'
--        -> Writer [Bracelet] ()
--     go t p r z b rs content pre
--       | lastCount == Just (n - t + 1) = do
--           let (sb1,vb1) = getBlock pre (b+1)
--               rs'' | (lastCount > 0) && (r+1 /= t) && (sb1 == k-1) && (vb1 > lastCount)
--                    = True
--                    | (lastCount > 0) && (r+1 /= t) && (sb1 /= k-1 || vb1 < lastCount)
--                    = False
--                    | otherwise = rs'
--           when (not rs'') $ candidate (if lastCount > runtp then n else p)  -- XXX runtp
--       | otherwise
--       = when (IM.lookup 0 content /= n - t + 1) $ do
--           return ()   -- XXX
--           -- j := head   -- head = next thing in the linked list.

--       where
--         rs' | (t-1 > (n-r) `div` 2 + r) && pre ! (t-1) > pre ! (n-t+2+r) = False
--             | (t-1 > (n-r) `div` 2 + r) && pre ! (t-1) < pre ! (n-t+2+r) = True
--             | otherwise = rs
--         lastCount = IM.lookup (k-1) content

----------------------------------------------------------------------
----------------------------------------------------------------------

main :: IO ()
main = do
  [n, con] <- getArgs
  let bs = genFixedBracelets (read n) (read con)
--  print bs
  print (length bs)
