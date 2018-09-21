main :: IO ()
main = do
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

maybeLandLeft :: Birds -> Pole -> Maybe Pole  
maybeLandLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
maybeLandRight :: Birds -> Pole -> Maybe Pole  
maybeLandRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  

{-
APPLICATIVE
===========

t1 (a -> b) -> t2 a -> t2 b


-}
