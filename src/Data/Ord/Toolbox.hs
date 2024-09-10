-- |
-- Module       : Data.Ord.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Data.Ord'.
--
-- This module re-exports the above module, so modules need only import 'Data.Ord.Toolbox'.
module Data.Ord.Toolbox (
  -- * Convenience functions
  minBy,
  maxBy,
  minOn,
  maxOn,

  -- * Re-exports
  module Data.Ord,
) where

import Data.Ord

-- | Choose the smallest of two values when compared with the given function.
--
-- > minBy compare == min
minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy f a b = case f a b of
  GT -> b
  _ -> a

-- | Choose the largest of two values when compared with the given function.
--
-- > maxBy compare == max
maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy f a b = case f a b of
  GT -> a
  _ -> b

-- | Choose the argument minimum of the given function.
--
-- > minOn negate == max
minOn :: (Ord b) => (a -> b) -> a -> a -> a
minOn f a b = case comparing f a b of
  GT -> b
  _ -> a

-- | Choose the argument maximum of the given function.
--
-- > maxOn negate == min
maxOn :: (Ord b) => (a -> b) -> a -> a -> a
maxOn f a b = case comparing f a b of
  GT -> a
  _ -> b
