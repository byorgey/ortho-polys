{-# LANGUAGE RankNTypes #-}

import           Control.Monad              (forM_, when)
import           Control.Monad.ST
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Writer
import           Data.Array.Base            (unsafeRead, unsafeWrite)
import           Data.Array.ST
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

main :: IO ()
main = do
  [k, n] <- getArgs
  let ns = genNecklaces (read k) (read n)
  -- print ns
  print (length ns)
