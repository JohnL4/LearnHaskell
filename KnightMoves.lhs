\documentclass[11pt,letterpaper]{article}

% Process with command: pdflatex <this-file>

\usepackage{verbatim}
\newenvironment{code}{\footnotesize\verbatim}{\endverbatim\normalsize}

%opening
\title{Deciphering Haskell monadic {\tt do}}
\author{John Lusk}

\begin{document}

\maketitle

\begin{abstract}
A stupid litle exercise in which I try to figure out how {\tt do} really works in the solution presented in the ``knight's
quest'' section of the chapter ``A Fistful of Monads'' of the book \emph{Learn You a Haskell For Great Good!}.
\end{abstract}


The book treats lists as ``indeterminate answers'' (i.e., a list of possible values that the ``answer'' could be, as opposed
to a single ``answer'' value).

It also uses {\tt List} as a {\tt Monad}, along with {\tt do}, so I wanted to try to figure all this out.

\section{Original Code, with Some Re-writing}

I start with the code from {\em Learn You a Haskell}, and do a little re-writing to eliminate {\tt do}.

\subsection{Main Code}

Some initial setup:

\begin{code}
module KnightMoves where

import Control.Monad

-- Modules for unit test
import Test.Hspec
import Data.Set as Set

-- | (Column, Row)
type KnightPos = (Int,Int)
\end{code}

Here is the original definition of {\tt moveKnight} from the book:

\begin{code}
-- | The original function from Learn You a Haskell
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')  
\end{code}

What the heck is is going on here?  The first thing I did was to move that long list out of the code body, using a
{\tt where} clause to simplify things, hence the {\tt w} in the function name:

\begin{code}
-- | "moveKnight where" -- first re-write
mkw :: KnightPos -> [KnightPos]
mkw (c,r) = do  
  (c',r') <- moves
  guard (c' `elem` [1..8] && r' `elem` [1..8])  
  return (c',r')  
  where
    moves = [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]
\end{code}

Then, we rewrite to translate the {\tt do} into what it represents ({\tt >>=} and {\tt >>} operators).

We also define a
special {\tt gard} function that does the same thing as {\tt guard}.  Remember, {\tt guard}'s job is to yield either
{\tt mzero} (from {\tt MonadPlus}), which is just {\tt []} in this case, or {\tt return ()}, which, in the case of the
{{\tt List} monad, is just {\tt [()]}, the list containing a single empty tuple (also known as ``unit'').

The use of {\tt >>} after {\tt gard} will either substitute another value for the incoming value (if it's {\tt [()]}, or
fail to do anything if the incoming value is the empty list {\tt []} (because we're mapping a function to the incoming
list, which, if empty, results in an empty list, no matter what the function is).

Here's the code:

\begin{code}
-- | 2nd rewrite, without "do"
mkw2 :: KnightPos -> [KnightPos]
mkw2 (c,r) =
  -- do e1 ; e2      =        e1 >> e2
  -- do p <- e1; e2  =        e1 >>= \p -> e2
  moves >>= \(c',r') -> gard (c',r') >> [(c',r')]
  where
    moves = [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]
    gard (c',r') = if (c' `elem` [1..8] && r' `elem` [1..8])
                   then [()]
                   else []
\end{code}

Then we do the exact same thing with another function, {\tt in3}:

\begin{code}
in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- mkw2 start          -- was: moveKnight, not mkw
    second <- mkw2 first
    mkw2 second
\end{code}

The rewrite of this function looks pretty much the same as that for {\tt moveKnight}, except there is no occurrence of
the {\tt >>} operator:

\begin{code}
-- | Re-write, without "do"
in32 :: KnightPos -> [KnightPos]
in32 start =
  mkw2 start >>= \first -> mkw2 first >>= \second -> mkw2 second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
\end{code}

\subsection{Unit Test}

The best way, in the long run, to test that we've rewritten the functions properly, as we iteratively edit and test our
code, is to simply assert that we have done so.  To that end, we write easily-repeatable test code.

\begin{code}
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

\section{Explanation of How It All Works}

\subsection{{\tt MoveKnight} (or {\tt mkw2})}

As you recall, the definitions for {\tt >>=} and {\tt >>} (for {\tt List}) are as follows:
\begin{verbatim}
xs >>= f = concat (map f xs)}
xs >> ys = xs >>= \_ -> ys
\end{verbatim}

{\tt >>} can be rewritten as {\tt xs >> ys = concat (map (\_ -> ys) xs)}.  You can see that {\tt >>} will substitute
{\tt ys} for {\tt xs} only if {\tt xs} is non-empty (or non-{\tt fail}, since {\tt fail} is the empty list).  Otherwise,
{\tt >>} does nothing, and the result is still {\tt []}.

In {\tt moveKnight}, we feed an entire list of (possible) knight positions (column, row) into a function, which, {\em for each
element of the list} (by virtue of {\tt map}), uses the {\tt guard} ({\tt gard}) function to yield either {\tt []} (if
the condition fails) or {\tt [()]}, which is {\em not} an empty list (if the condition succeeds).  That {\tt guard}
result is then fed through {\tt >>}, which attempts to substitute a single-element list containing the original input
value ({\tt (c',r')}, an element of the original list of knight positions).  That substitution will fail if {\tt guard}
failed, since mapping a function to an empty list results in an empty list, no matter what the function is.

The result of this giant function mapped to a list of knight positions is a list of singleton lists that looks something
like this:

\begin{verbatim}
[ [(1,1)], [(2,2)], [], [(4,4)] ]
\end{verbatim}

We ``flatten'' this list out by applying {\tt concat} to it, which concatenates all the lists into one big list,
resulting in

\begin{verbatim}
[ (1,1), (2,2), (4,4) ]
\end{verbatim}

\subsection{{\tt in3} or {\tt in32}}

{\tt in3} ({\tt in32}) begins with the result of {\tt moveKnight} ({\tt mkw2}), a monad ({\tt List}), and binds it
({\tt >>=}, ``bind'') to a giant function.  This function is applied to {\em each member} (labelled {\tt first} of the
list generated by 
{\tt moveKnight}, per the definition of {\tt >>=}.

{\tt first} is itself passed to {\tt moveKnight} (again), which generates a {\em new} list of positions which is in turn bound
to a new (nested) function, which processes each element of the moves generated from {\tt first}.  (The elements of the
second list are named {\tt second} when passed to the bound function.)

{\tt moveKnight} is called for a third time on the elements of the second list (each elementwise named {\tt second}), to
generate a possible list of moves from the second move.

At the end of all this, we've called {\tt moveKnight} three times, constantly expanding the list of possible 3-move
positions of the knight.  (Remember, we're calling {\tt concat . map} all the time, so we're constantly flattening lists
of lists.)

At the end of all this, we have a list of moves possible by moving the knight three times.


\end{document}
