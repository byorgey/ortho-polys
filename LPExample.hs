-- from glpk-hs package
import           Control.Monad.LPMonad
import           Data.LinearProgram.Common

-- Solve example on Integer programming wikipedia page.

main :: IO ()
main = do
  (rc,vals) <- evalLPT $ do
        setDirection Max
        setObjective (var "y")
        leqTo (linCombination [(-1 :: Double, "x"),(1,"y")]) 1
        leqTo (linCombination [(3,"x"), (2,"y")]) 12
        leqTo (linCombination [(2,"x"), (3,"y")]) 12
        varGeq "x" 0
        varGeq "y" 0
        setVarKind "x" IntVar
        setVarKind "y" IntVar
        quickSolveMIP
  print rc
  print vals
