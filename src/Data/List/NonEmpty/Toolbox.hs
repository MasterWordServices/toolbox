-- |
-- Module       : Data.List.NonEmpty.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Data.List.NonEmpty'.
--
-- This module re-exports the above module, so modules need only import 'Data.List.NonEmpty.Toolbox'.
module Data.List.NonEmpty.Toolbox (
  -- * Quasi-constructors
  (|:),
  (|>),
  (<||),
  (||>),

  -- * Using regular lists
  withNonEmpty,
  whenNonEmpty,
  whenNonEmptyM,

  -- * 'Foldable' specializations
  fold1,
  foldMap1,
  foldMap1',
  maximum1,
  minimum1,
  maximumBy1,
  minimumBy1,
  maximumOn1,
  minimumOn1,
  maximumOf1,
  minimumOf1,

  -- * List functions
  sortOn,
  union,
  unionBy,
  unionOn,
  intersect,
  intersectBy,
  groupAll,

  -- * Re-exports
  module Data.List.NonEmpty,
) where

import Control.Monad.Toolbox
import qualified Data.Foldable.Toolbox as FT
import Data.Function.Toolbox (using)
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import qualified Data.List.Toolbox as LT
import Data.Maybe (fromJust)
import Prelude hiding (maximum, minimum)

infixl 5 |:, |>

-- | Append an element to a list.
--
-- > [1, 2, 3] |: 4 == 1 :| [2, 3, 4]
(|:) :: [a] -> a -> NonEmpty a
xs |: x = foldr cons (pure x) xs

-- | Append an element to a 'NonEmpty' list.
--
-- > (1 :| [2, 3]) |> 4 == 1 :| [2, 3, 4]
(|>) :: NonEmpty a -> a -> NonEmpty a
xs |> x = xs <> pure x

-- | Append a list to a 'NonEmpty' list.
--
-- > (1 :| [2, 3]) ||> [4, 5] == 1 :| [2, 3, 4, 5]
(||>) :: NonEmpty a -> [a] -> NonEmpty a
(x :| xs) ||> lx = x :| xs <> lx

-- | Append a 'NonEmpty' list to a list.
--
-- > [1, 2, 3] <|| (4 :| [5]) == 1 :| [2, 3, 4, 5]
(<||) :: [a] -> NonEmpty a -> NonEmpty a
lx <|| nx = foldr cons nx lx

-- | Use a 'NonEmpty' function on a regular list safely.
--
-- > withNonEmpty [1, 2, 3] maximum1 == Just 3
-- > withNonEmpty [] maximum1 == Nothing
withNonEmpty :: [a] -> (NonEmpty a -> b) -> Maybe b
withNonEmpty xs f = f <$> nonEmpty xs

-- | A version of 'Control.Monad.Toolbox.whenJust' for lists.
whenNonEmpty :: (Monad m) => [a] -> (NonEmpty a -> m ()) -> m ()
whenNonEmpty xs f = whenJust (nonEmpty xs) f

-- | A version of 'Control.Monad.Toolbox.whenJust' for lists.
whenNonEmptyM :: (Monad m) => m [a] -> (NonEmpty a -> m ()) -> m ()
whenNonEmptyM xs f = whenJustM (nonEmpty <$> xs) f

-- | A specialized version of 'fold1' to 'NonEmpty' lists.
fold1 :: (Semigroup m) => NonEmpty m -> m
fold1 xs = fromJust $ FT.fold1 xs

-- | A specialized version of 'foldMap1' to 'NonEmpty' lists.
foldMap1 :: (Semigroup m) => (a -> m) -> NonEmpty a -> m
foldMap1 f xs = fromJust $ FT.foldMap1 f xs

-- | A specialized version of 'foldMap1' to 'NonEmpty' lists.
foldMap1' :: (Semigroup m) => (a -> m) -> NonEmpty a -> m
foldMap1' f xs = fromJust $ FT.foldMap1' f xs

-- | A specialized version of 'Data.List.maximum' to 'NonEmpty' lists.
--
-- > maximum1 (1 :| [2, 3, 4]) == 4
maximum1 :: (Ord a) => NonEmpty a -> a
maximum1 xs = fromJust $ FT.maximumOn id xs

-- | A specialized version of 'Data.List.minimum' to 'NonEmpty' lists.
--
-- > minimum1 (1 :| [2, 3, 4]) == 1
minimum1 :: (Ord a) => NonEmpty a -> a
minimum1 xs = fromJust $ FT.minimumOn id xs

-- | A specialized version of 'Data.List.maximumBy' to 'NonEmpty' lists.
--
-- > maximumBy1 (comparing negate) (1 :| [2, 3, 4]) == 1
maximumBy1 :: (a -> a -> Ordering) -> NonEmpty a -> a
maximumBy1 xs = FT.maximumBy xs

-- | A specialized version of 'Data.List.minimumBy' to 'NonEmpty' lists.
--
-- > minimumBy1 (comparing negate) (1 :| [2, 3, 4]) == 4
minimumBy1 :: (a -> a -> Ordering) -> NonEmpty a -> a
minimumBy1 xs = FT.minimumBy xs

-- | A specialized version of 'Data.Foldable.Toolbox.maximumOn' to 'NonEmpty' lists.
--
-- > maximumOn1 negate (1 :| [2, 3, 4]) == -1
maximumOn1 :: (Ord b) => (a -> b) -> NonEmpty a -> a
maximumOn1 f xs = fromJust $ FT.maximumOn f xs

-- | A specialized version of 'Data.Foldable.Toolbox.minimumOn' to 'NonEmpty' lists.
--
-- > minimumOn1 negate (1 :| [2, 3, 4]) == -4
minimumOn1 :: (Ord b) => (a -> b) -> NonEmpty a -> a
minimumOn1 f xs = fromJust $ FT.minimumOn f xs

-- | A specialized version of 'Data.Foldable.Toolbox.maximumOf' to 'NonEmpty' lists.
--
-- > maximumOf1 negate (1 :| [2, 3, 4]) == -1
maximumOf1 :: (Ord b) => (a -> b) -> NonEmpty a -> b
maximumOf1 f xs = fromJust $ FT.maximumOf f xs

-- | A specialized version of 'Data.Foldable.Toolbox.minimumOf' to 'NonEmpty' lists.
--
-- > minimumOf1 negate (1 :| [2, 3, 4]) == -4
minimumOf1 :: (Ord b) => (a -> b) -> NonEmpty a -> b
minimumOf1 f xs = fromJust $ FT.minimumOf f xs

-- | A version of 'Data.List.sortOn' for 'NonEmpty' lists.
sortOn :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn f xs = fromList . LT.sortOn f $ FT.toList xs

-- | A version of 'Data.List.union' for 'NonEmpty' lists.
union :: (Eq a) => NonEmpty a -> NonEmpty a -> NonEmpty a
union xs ys = unionBy (==) xs ys

-- | A version of 'Data.List.Toolbox.unionBy' for 'NonEmpty' lists.
unionBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a -> NonEmpty a
unionBy f xs ys = fromList $ (LT.unionBy f `using` toList) xs ys

-- | A version of 'Data.List.Toolbox.unionOn' for 'NonEmpty' lists.
unionOn :: (Eq b) => (a -> b) -> NonEmpty a -> NonEmpty a -> NonEmpty a
unionOn f xs ys = unionBy ((==) `using` f) xs ys

groupAll :: (Ord a) => [a] -> [NonEmpty a]
groupAll = NE.groupAllWith id

-- | A version of 'Data.List.intersect' for 'NonEmpty' lists.
--
--   Note that the 'NonEmpty' guarantee is not preserved.
--
-- > intersect (1 :| [2, 3]) (2 :| [3, 4]) == [2, 3]
-- > intersect (1 :| [2, 3]) (4 :| [5, 6]) == []
intersect :: (Eq a) => NonEmpty a -> NonEmpty a -> [a]
intersect xs ys = (LT.intersect `using` toList) xs ys

-- | A version of 'Data.List.intersectBy' for 'NonEmpty' lists.
--
--   Note that the 'NonEmpty' guarantee is not preserved.
intersectBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a -> [a]
intersectBy f xs ys = (LT.intersectBy f `using` toList) xs ys
