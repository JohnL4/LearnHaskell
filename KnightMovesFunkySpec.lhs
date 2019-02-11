
\begin{code}
module KnightMovesFunkySpec where

import KnightMoves

import Test.Hspec
import Data.Set as Set

twoReturns :: [String]
twoReturns = do
  return "aaa"
  return "bbb"

main :: IO ()
main = do
  putStrLn $ "twoReturns -> " ++ show twoReturns
\end{code}

% Local Variables:
% mode: latex
% mmm-classes: literate-haskell-latex
% End:
