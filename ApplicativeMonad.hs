-- If you are reading this in conjunction with ApplicativeMonadSpec.hs, skip down to where I define class MMaybe and
-- function `land`.

module ApplicativeMonad where

demo :: IO ()
demo = do
  putStrLn $ (++) "Pole after unchecked landings: " $ show
    $ initial -: landLeft 1 -: landRight 1 -: landLeft 2
  putStrLn $ (++) "Pole after unchecked landings which SHOULD cause an error: " $ show
    $ initial -: landLeft 1 -: landRight 4 -: landLeft (-1) -- Error should occur here -- pole is unbalanced.
    -: landRight (-2)  
  where
    initial = (0,0)
  
-- Function application looks more like OO method calls; allows more graceful (fluent?) chaining.
(-:) :: a -> (a -> b) -> b
x -: f = f x

type Birds = Int                -- Really, this should be BirdCount, but, since I'm copying code from Learn You A
                                -- Haskell, I'll just leave it this way.
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole  
landLeft n (left,right) = (left + n,right)  
  
landRight :: Birds -> Pole -> Pole  
landRight n (left,right) = (left,right + n)  

-- These "maybe" functions become the "a -> m a" part of the Monad definition of ">>=".  Unused in this example.

maybeLandLeft :: Birds -> Pole -> Maybe Pole  
maybeLandLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
maybeLandRight :: Birds -> Pole -> Maybe Pole  
maybeLandRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  

----------------------------------------------------------------

{-
MONAD
=====

class (Applicative m) => Monad m where
(>>=) :: m a -> (a -> m b) -> m b

APPLICATIVE
===========

(<*>) :: fr (a -> b) -> fr a -> fr b

Where 'fr' is a Functor.

FUNCTOR
=======

As a reminder, a Functor is a thing that can be mapped over (e.g., [a], Tree a, Maybe a, etc.)

Has one function, fmap :: (a -> b) -> fr a -> fr b.

PIERRE
======

This only works because we use Maybe to pass state.  It has nothing to do with >>=.
-}

-- We define our own version of Maybe so we don't drag in all of Maybe's standard definitions (Functor, Applicative,
-- Monad, etc.)

data MMaybe a = NNothing | JJust a
  deriving (Eq, Show, Ord)

-- Note that this version of MMaybe is not even a Functor, at this point.

data Side = LeftSide | RightSide

land :: Side -> Birds -> MMaybe Pole -> MMaybe Pole
land _ _ NNothing = NNothing
land LeftSide n (JJust (left, right)) 
  | (abs (left + n - right) > 3) = NNothing
  | otherwise                    = JJust (left + n, right)
land RightSide n (JJust (left, right))
  | (abs (left - (right + n)) > 3)  = NNothing
  | otherwise                       = JJust (left, right + n)
