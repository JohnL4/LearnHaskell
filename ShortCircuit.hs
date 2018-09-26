{-
Exploring the ">>" Monad operator.
-}

module ShortCircuit where

data MMaybe a = NNothing | JJust a
  deriving (Eq, Show, Ord)

-- Monad requires that MMaybe be an Applicative functor.
instance Monad MMaybe where
  return x        = JJust x
  NNothing >>= _  = NNothing    -- Monad default definition of (>>) is x >> y = x >>= \_ -> y.  So, you can see why
                                -- NNothing >>= (\_ -> y) yields NNothing (because >>= just doesn't care about whatever
                                -- (\_ -> y) is.  So, NNothing >> JJust 3 yields NNothing, not JJust 3
  JJust x >>= f   = f x
  fail _          = NNothing

-- Applicative requires that MMaybe be a Functor
instance Applicative MMaybe where
  pure                    = JJust
  NNothing <*> _          = NNothing
  (JJust f) <*> something = fmap f something

-- Must declare MMaybe to be a Functor before we can declare it to be Applicative or Monad.
instance Functor MMaybe where
  fmap f (JJust x) = JJust (f x)
  fmap _ NNothing  = NNothing

