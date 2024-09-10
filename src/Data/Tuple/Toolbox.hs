{-# LANGUAGE TupleSections #-}

-- |
-- Module       : Data.Tuple.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Data.Tuple'.
--
-- This module re-exports the above library, so modules need only import 'Data.Tuple.Toolbox'.
module Data.Tuple.Toolbox (
  -- * Pairs
  dup,
  (&&&),
  (***),
  first,
  second,
  both,
  assoc,
  unassoc,
  firstF,
  secondF,
  toFirst,
  toSecond,

  -- * Triples
  fst3,
  snd3,
  thd3,
  first3,
  second3,
  third3,
  first3F,
  second3F,
  third3F,
  pairFst,
  unpairFst,
  pairSnd,
  unpairSnd,

  -- * 4-tuples
  frst4,
  scnd4,
  thrd4,
  frth4,
  first4,
  second4,
  third4,
  fourth4,
  first4F,
  second4F,
  third4F,
  fourth4F,

  -- * Re-exports
  module Data.Tuple,
) where

import Control.Monad (join)
import qualified Data.Bifunctor (bimap, first, second)
import Data.Tuple

-- | Duplicate a value.
dup :: a -> (a, a)
dup a = (a, a)

infix 1 &&&, ***

-- | Apply two functions to the same value.
--
-- > (pred &&& succ) 0 == (-1, 1)
(&&&) :: (a -> b) -> (a -> c) -> (a -> (b, c))
(&&&) = liftA2 (,)

-- | Apply two functions pairwise.
--
-- > (pred *** succ) (0, 0) == (-1, 1)
(***) :: (a -> c) -> (b -> d) -> ((a, b) -> (c, d))
(***) = Data.Bifunctor.bimap

-- | Apply the same function to both components.
both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)

-- | Map a function over the first component of a pair.
first :: (a -> b) -> (a, x) -> (b, x)
first = Data.Bifunctor.first

-- | Map a function over the second component of a pair.
second :: (a -> b) -> (x, a) -> (x, b)
second = Data.Bifunctor.second

-- | Map a functorial function over the first component a pair, so that the whole pair is inside the functor.
firstF :: (Functor f) => (a -> f b) -> (a, x) -> f (b, x)
firstF f (a, x) = (,x) <$> f a

-- | Map a functorial function over the second component a pair, so that the whole pair is inside the functor.
secondF :: (Functor f) => (a -> f b) -> (x, a) -> f (x, b)
secondF f (x, a) = (x,) <$> f a

-- | Apply a functorial function and keep the argument in the second component.
--
-- > toFirst (:[]) 3 == ([3], 3)
-- > toFirst f x == firstF (const . f) (dup x)
toFirst :: (Functor f) => (a -> f b) -> a -> f (b, a)
toFirst f a = (,a) <$> f a

-- | Apply a functorial function and keep the argument in the first component.
--
-- > toSecond (:[]) 3 == ([3], 3)
-- > toSecond f x == secondF (const . f) (dup x)
toSecond :: (Functor f) => (a -> f b) -> a -> f (a, b)
toSecond f a = (a,) <$> f a

-- | Associate a pair-inside-a-pair.
--
-- > assoc . unassoc == id
assoc :: (a, (b, c)) -> ((a, b), c)
assoc (a, (b, c)) = ((a, b), c)

-- | Un-associate a pair-inside-a-pair.
--
-- > unassoc . assoc == id
unassoc :: ((a, b), c) -> (a, (b, c))
unassoc ((a, b), c) = (a, (b, c))

-- | Flatten a 'fst'-nested pair into a 3-tuple.
unpairFst :: ((a, b), c) -> (a, b, c)
unpairFst ((a, b), c) = (a, b, c)

-- | Flatten a 'snd'-nested pair into a 3-tuple.
unpairSnd :: (a, (b, c)) -> (a, b, c)
unpairSnd (a, (b, c)) = (a, b, c)

-- | Associate a 3-tuple into a 'fst'-nested pair.
pairFst :: (a, b, c) -> ((a, b), c)
pairFst (a, b, c) = ((a, b), c)

-- | Associate a 3-tuple into a 'snd'-nested pair.
pairSnd :: (a, b, c) -> (a, (b, c))
pairSnd (a, b, c) = (a, (b, c))

-- | Get the first component of a triple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Get the second component of a triple.
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | Get the third component of a triple.
thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- | Map a function over the first component of a triple.
--
-- > first3 succ (0, 0, 0) == (1, 0, 0)
first3 :: (a -> x) -> (a, b, c) -> (x, b, c)
first3 f (a, b, c) = (f a, b, c)

-- | Map a function over the second component of a triple.
--
-- > second3 succ (0, 0, 0) == (0, 1, 0)
second3 :: (b -> x) -> (a, b, c) -> (a, x, c)
second3 f (a, b, c) = (a, f b, c)

-- | Map a function over the third component of a triple.
--
-- > third3 succ (0, 0, 0) == (0, 0, 1)
third3 :: (c -> x) -> (a, b, c) -> (a, b, x)
third3 f (a, b, c) = (a, b, f c)

-- | Map a functorial function over the first component of a triple, so that the whole triple is inside the functor.
--
-- > first3F (const Nothing) (0, 0, 0) == Nothing
first3F :: (Functor f) => (a -> f x) -> (a, b, c) -> f (x, b, c)
first3F f (a, b, c) = (,b,c) <$> f a

-- | Map a functorial function over the second component of a triple, so that the whole triple is inside the functor.
--
-- > second3F (Just . pred) (0, 0, 0) == Just (0, -1, 0)
second3F :: (Functor f) => (b -> f x) -> (a, b, c) -> f (a, x, c)
second3F f (a, b, c) = (a,,c) <$> f b

-- | Map a functorial function over the third component of a triple, so that the whole triple is inside the functor.
--
-- > third3F (Just . succ) (0, 0, 0) == Just (0, 0, 1)
third3F :: (Functor f) => (c -> f x) -> (a, b, c) -> f (a, b, x)
third3F f (a, b, c) = (a,b,) <$> f c

-- | Get the first component of a 4-tuple.
frst4 :: (a, b, c, d) -> a
frst4 (a, _, _, _) = a

-- | Get the second component of a 4-tuple.
scnd4 :: (a, b, c, d) -> b
scnd4 (_, b, _, _) = b

-- | Get the third component of a 4-tuple.
thrd4 :: (a, b, c, d) -> c
thrd4 (_, _, c, _) = c

-- | Get the fourth component of a 4-tuple.
frth4 :: (a, b, c, d) -> d
frth4 (_, _, _, d) = d

-- | Map a function over the first component of a 4-tuple.
--
-- > first4 succ (0, 0, 0, 0) == (1, 0, 0, 0)
first4 :: (a -> x) -> (a, b, c, d) -> (x, b, c, d)
first4 f (a, b, c, d) = (f a, b, c, d)

-- | Map a function over the second component of a 4-tuple.
--
-- > second4 succ (0, 0, 0, 0) == (0, 1, 0, 0)
second4 :: (b -> x) -> (a, b, c, d) -> (a, x, c, d)
second4 f (a, b, c, d) = (a, f b, c, d)

-- | Map a function over the third component of a 4-tuple.
--
-- > third4 succ (0, 0, 0, 0) == (0, 0, 1, 0)
third4 :: (c -> x) -> (a, b, c, d) -> (a, b, x, d)
third4 f (a, b, c, d) = (a, b, f c, d)

-- | Map a function over the fourth component of a 4-tuple.
--
-- > fourth4 succ (0, 0, 0, 0) == (0, 0, 0, 1)
fourth4 :: (d -> x) -> (a, b, c, d) -> (a, b, c, x)
fourth4 f (a, b, c, d) = (a, b, c, f d)

-- | Map a function over the first component of a 4-tuple, so that the entire tuple is inside the functor.
first4F :: (Functor f) => (a -> f x) -> (a, b, c, d) -> f (x, b, c, d)
first4F f (a, b, c, d) = (,b,c,d) <$> f a

-- | Map a function over the second component of a 4-tuple, so that the entire tuple is inside the functor.
second4F :: (Functor f) => (b -> f x) -> (a, b, c, d) -> f (a, x, c, d)
second4F f (a, b, c, d) = (a,,c,d) <$> f b

-- | Map a function over the third component of a 4-tuple, so that the entire tuple is inside the functor.
--
-- > third4F succ (0, 0, 0, 0) == (0, 0, 1, 0)
third4F :: (Functor f) => (c -> f x) -> (a, b, c, d) -> f (a, b, x, d)
third4F f (a, b, c, d) = (a,b,,d) <$> f c

-- | Map a function over the fourth component of a 4-tuple, so that the entire tuple is inside the functor.
fourth4F :: (Functor f) => (d -> f x) -> (a, b, c, d) -> f (a, b, c, x)
fourth4F f (a, b, c, d) = (a,b,c,) <$> f d
