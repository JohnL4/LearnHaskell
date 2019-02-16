\subsection{Unit Test}

The best way, in the long run, to test that we've rewritten the functions properly, as we iteratively edit and test our
code, is to simply assert that we have done so.  To that end, we write easily-repeatable test code.

\begin{code}
import KnightMoves
  
import Test.Hspec
import Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "various implementations" $ do
    it "moveKnight == mkw" $
\end{code}

(Here follows the actual assertion:)

\begin{code}
      (Set.fromList $ moveKnight (5,5)) == (Set.fromList $ mkw (5,5))
    it "mkw == mkw2" $
      (Set.fromList $ mkw (5,5)) == (Set.fromList $ mkw2 (5,5))
    it "in3 == in32" $
      (Set.fromList $ in3 (5,5)) == (Set.fromList $ in32 (5,5))
\end{code}

% Local Variables:
% mode: latex
% mmm-classes: literate-haskell-latex
% End:
