-- Simple test to see which way the Writer monad associates calls to
-- mappend

import           Control.Monad.Trans.Writer
import           Data.Monoid

data T = E | L Int | N T T
  deriving Show

instance Monoid T where
  mempty = E
  mappend = N

f :: Writer T ()
f = do
  tell (L 1)
  tell (L 2)
  tell (L 3)
  tell $ L 4
  return ()

main :: IO ()
main = do
  let ((),t) = runWriter f
  print t

{-
eudoxus :: ~/research/ortho-polys » runhaskell WriterTest.hs
N (L 1) (N (L 2) (N (L 3) (N (L 4) E)))
-}

-- Good, they are associated to the right.  So using e.g.  Writer [a]
-- b does not cause O(n^2) left-nested list appends.
