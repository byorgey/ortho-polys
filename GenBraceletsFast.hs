import GenBracelets
import System.Environment

main = do
  [n] <- getArgs
  print (length (genFixedBracelets (2*n+4) [(0,n), (1,n+4)]))
