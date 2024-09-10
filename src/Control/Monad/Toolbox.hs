-- |
-- Module       : Control.Monad.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Control.Applicative' and 'Control.Monad'.
--
-- This module re-exports the above modules, so modules need only import 'Control.Monad.Toolbox'.
module Control.Monad.Toolbox (
  -- * Functorial operators
  (<<$>>),

  -- * Applicative operators
  guarded,

  -- ** Booleans
  ifA,
  (<&&>),
  (<||>),

  -- ** Lifted eliminators
  maybeA,
  fromMaybeA,
  eitherA,

  -- * Monad operators

  -- ** Booleans
  ifM,
  (&^&),
  (|^|),

  -- ** Lifted eliminators
  maybeM,
  fromMaybeM,
  eitherM,

  -- ** Monad utilities
  whenM,
  unlessM,
  whenJust,
  whenJustM,

  -- ** Looping
  whileM,
  loop,
  loopM,

  -- * Re-exports
  module Control.Monad,
  module Control.Applicative,
  module Data.Functor,
) where

import Control.Applicative
import Control.Monad
import Data.Bool (bool)
import Data.Function (fix)
import Data.Functor
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromMaybe)

-- | Apply a pure function through two functors instead of one.
(<<$>>) :: (Functor g, Functor f) => (a -> b) -> g (f a) -> g (f b)
f <<$>> gfa = fmap (fmap f) gfa

-- | A version of @if...then...else@ over an 'Applicative' functor. Does not short-circuit!
--
--   If you need short-circuiting and have a 'Monad' constraint, use 'ifM'.
--
-- > ifA (Just False) (Just 1) Nothing == Nothing
-- > ifA (Just True) (Just 1) Nothing == Just 1
-- > ifA (Just True) (Just 1) (Just undefined) == undefined
ifA :: (Applicative f) => f Bool -> f a -> f a -> f a
ifA test t f = bool <$> f <*> t <*> test

infixr 3 <&&>

-- | A version of '(&&)' over an @Applicative functor. Does not short-circuit!
--
--   If you need short-circuiting and have a 'Monad' constraint, use '(&^&)'.
--
-- > Just True <&&> Just False == Just False
-- > Just False <&&> Just undefined == undefined
(<&&>) :: (Applicative f) => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

infixr 2 <||>

-- | A version of '(||)' over an @Applicative functor. Does not short-circuit!
--
--   If you need short-circuiting and have a 'Monad' constraint, use '(|^|)'.
--
-- > Just True <||> Just False == Just True
-- > Just True <||> Just undefined == undefined
(<||>) :: (Applicative f) => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

-- | A version of @if...then...else@ over a 'Monad'. Short-circuits if the 'Bool' is true.
--
-- > ifM (Just False) (Just 1) (Just 0) == Just 0
-- > ifM (Just True) (Just 1) (Just 0) == Just 1
-- > ifM (Just True) (Just 1) (Just undefined) == Just 1
ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM test t f = bool f t =<< test

infixr 3 &^&

-- | A version of '(&&)' over a 'Monad'. Short-circuits if the first argument is 'False'.
--
-- > Just True &^& Just False == Just False
-- > Just False &^& Just undefined == Just False
(&^&) :: (Monad m) => m Bool -> m Bool -> m Bool
ma &^& mb = ifM ma mb (pure False)

infixr 2 |^|

-- | A version of '(&&)' over a 'Monad'. Short-circuits if the first argument is 'True'.
--
-- > Just True |^| Just False == Just True
-- > Just True |^| Just undefined == Just True
(|^|) :: (Monad m) => m Bool -> m Bool -> m Bool
ma |^| mb = ifM ma (pure True) mb

-- | A version of 'when' where the test can be inside the 'Monad'.
--
-- > whenM [True] [(), (), ()] == [(), (), ()]
-- > whenM [False] [(), (), ()] == [()]
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM test act = ifM test act (pure ())

-- | A version of 'unless' where the test can be inside the 'Monad'.
--
-- > unlessM [True] [(), (), ()] == [()]
-- > unlessM [False] [(), (), ()] == [(), (), ()]
unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM test act = ifM (not <$> test) act (pure ())

-- | A version of 'when' that does nothing when given 'Nothing', and provides access
--   to the wrapped value otherwise.
--
-- > whenJust (Just 5) (\_ -> [(), (), ()]) == [(), (), ()]
-- > whenJust Nothing (\_ -> [(), (), ()]) == [()]
whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (pure ())

-- | A combination of 'whenM' and 'whenJust'.
whenJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM = flip $ maybeM (pure ())

-- | A version of 'guard' that provides a value if the condition is met, or 'empty' otherwise.
--
-- > guarded @Maybe False 1 == Nothing
-- > guarded @Maybe True 1 == Just 1
guarded :: (Alternative f) => Bool -> a -> f a
guarded b = if b then pure else const empty

-- | A version of 'maybe' that works over an 'Applicative' functor.
--   Note the type of the "just" parameter is the same as the first parameter of '(<*>)'.
--
-- > maybeA [0] [\x -> x + 1, \x -> x + 2] [Just 2] == [3, 4]
-- > maybeA [0] [\x -> x + 1, \x -> x + 2] [Nothing] == [0]
maybeA :: (Applicative f) => f b -> f (a -> b) -> f (Maybe a) -> f b
maybeA noth just may = maybe <$> noth <*> just <*> may

-- | A version of 'fromMaybe' that works over an 'Applicative' functor.
--
-- > fromMaybeA [0] [Just 5] == [5]
-- > fromMaybeA [0] [Nothing] == [0]
fromMaybeA :: (Applicative f) => f a -> f (Maybe a) -> f a
fromMaybeA = liftA2 fromMaybe

-- | A version of 'maybe' that works over a 'Monad'.
--
-- > maybeM [0] (\x -> [x + 1, x + 2]) [Just 2] == [3, 4]
-- > maybeM [0] (\x -> [x + 1, x + 2]) [Nothing] == [0]
maybeM :: (Monad m) => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM noth just may = maybe noth just =<< may

-- | A restriction of 'fromMaybeA' to a 'Monad'.
fromMaybeM :: (Monad m) => m a -> m (Maybe a) -> m a
fromMaybeM = fromMaybeA

-- | A version of 'either' that works over an 'Applicative' functor.
--
-- > eitherA (ZipList [pred, (+3)]) (ZipList [succ, (*2)]) (ZipList [Left 5, Right 10]) == ZipList {getZipList = [4,20]}
-- > eitherA [pred, (+3)] [succ, (*2)] [Left 5, Right 10] == [4,11,4,20,8,11,8,20] -- FOIL
-- > eitherA [pred, (+3)] [] [Left 5, Right 10] == []
-- > eitherA (Just pred) (Just succ) (Just (Left 6)) == Just 5
-- > eitherA (Just pred) (Just succ) (Just (Right 6)) == Just 7
-- > eitherA (Just pred) (Just succ) (Nothing :: Maybe (Either Int Int)) == Nothing
-- > eitherA (Just pred) Nothing (Just (Left 6)) == Nothing
-- > eitherA Nothing (Just succ) (Just (Right 6)) == Nothing
eitherA :: (Applicative f) => f (a -> c) -> f (b -> c) -> f (Either a b) -> f c
eitherA = liftA3 either

-- | A version of 'either' that works over a 'Monad'.
--
-- > eitherM (\a -> [pred a, a + 3]) (\b -> [succ b, b * 2]) [Left 5, Right 10] == [4,8,11,20]
-- > eitherM (\_ -> Nothing) (\b -> Just (b + 2)) Nothing == Nothing
-- > eitherM (\_ -> Nothing) (\b -> Just (b + 2)) (Just (Left 5)) == Nothing
-- > eitherM (\_ -> Nothing) (\b -> Just (b + 2)) (Just (Right 5)) == Just 7
eitherM :: (Monad m) => (a -> m c) -> (b -> m c) -> m (Either a b) -> m c
eitherM lft rgt eab = either lft rgt =<< eab

-- | Run a monadic computation until its value becomes 'False'.
--
-- > execState (while (do {n <- get; if n < 10 then True <$ put (n + 1) else pure False} :: State Int Bool)) 0 == 10
whileM :: (Monad m) => m Bool -> m ()
whileM = fix (whenM <*>)

-- | Run a monadic computation until its value becomes 'Right'.
--
-- > loopM @Identity (\a -> if a < 0 then pure (Right a) else pure (Left $ pred a)) 20 == -1
loopM :: (Monad m) => (a -> m (Either a b)) -> a -> m b
loopM comp = eitherM (loopM comp) pure . comp

-- | Loop a computation using 'Left' values as seeds for the next iteration.
--
-- > loop (\a -> if a < 0 then Right a else Left (pred a)) 20 == -1
loop :: (a -> Either a b) -> a -> b
loop f = runIdentity . loopM (Identity . f)
