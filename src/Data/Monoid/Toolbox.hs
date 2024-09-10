-- |
-- Module       : Data.Monoid.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Data.Monoid'.
--
-- This module re-exports 'Data.Semigroup' (which exports better defaults than 'Data.Monoid'), so modules need only import 'Data.Monoid.Toolbox'.
module Data.Monoid.Toolbox (
  -- * General utilities
  discard,
  nomempty,
  (<?),
  (?>),

  -- * Re-exports
  module Data.Semigroup,
) where

import Data.Semigroup

-- | Discard a monoidal value if the condition succeeds.
--
-- >>> discard True [1] == []
-- >>> discard False [3] == [3]
discard :: (Monoid a) => Bool -> a -> a
discard b a = if b then mempty else a

-- | Returns 'Just' the value, provided it is not 'mempty'.
--
-- > nomempty [] == Nothing
-- > nomempty [4] == Just [4]
nomempty :: (Eq a, Monoid a) => a -> Maybe a
nomempty a = if a == mempty then Nothing else Just a

infix 0 <?, ?>

-- | Discard the value if the condition fails. Flipped version of @(?>)@.
--
-- > ([1, 2, 3] <? True) == [1, 2, 3]
-- > ([1, 2, 3] <? False) == []
(<?) :: (Monoid a) => a -> Bool -> a
a <? b = discard (not b) a

-- | Discard the value if the condition fails. Flipped version of @(<?)@.
--
-- > (True ?> [1, 2, 3]) == [1, 2, 3]
-- > (False ?> [1, 2, 3]) == []
(?>) :: (Monoid a) => Bool -> a -> a
b ?> a = discard (not b) a
