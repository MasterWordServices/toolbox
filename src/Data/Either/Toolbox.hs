-- |
-- Module       : Data.Either.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Data.Either'.
--
-- This module re-exports the above module, so modules need only import 'Data.Either.Toolbox'.
module Data.Either.Toolbox (
  -- * 'Control.Arrow' specializations
  (|||),
  (+++),

  -- * 'Maybe' conversions
  keepLeft,
  keepRight,
  maybeToLeft,
  maybeToRight,

  -- * Booleans
  boolToEither,

  -- * Re-exports
  module Data.Either,
) where

import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Either

-- | Infix synonym for 'either'.
(|||) :: (a -> c) -> (b -> c) -> Either a b -> c
(|||) = either

-- | Infix synonym for 'bimap'.
(+++) :: (a -> x) -> (b -> y) -> Either a b -> Either x y
(+++) = bimap

-- | Collapse an 'Either' to a 'Maybe', keeping only the 'Left' values.
--
-- > keepLeft (Left 5) == Just 5
-- > keepLeft (Right 1) == Nothing
keepLeft :: Either a b -> Maybe a
keepLeft = Just ||| const Nothing

-- | Collapse an 'Either' to a 'Maybe', keeping only the 'Right' values.
--
-- > keepRight (Left 5) == Nothing
-- > keepRight (Right 1) == Right 1
keepRight :: Either a b -> Maybe b
keepRight = const Nothing ||| Just

-- | Turn 'False' into a 'Left' and 'True' into a 'Right' by providing default values.
boolToEither :: a -> b -> Bool -> Either a b
boolToEither a b = bool (Left a) (Right b)

-- | Turn a 'Maybe' into an 'Either' by providing a default 'Right' value.
--
-- > maybeToLeft 4 Nothing == Right 4
-- > maybeToLeft 4 (Just 5) == Left 5
maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft b = maybe (Right b) Left

-- | Turn a 'Maybe' into an 'Either' by providing a default 'Left' value.
--
-- > maybeToRight 4 Nothing == Left 4
-- > maybeToRight 4 (Just 5) == Right 5
maybeToRight :: a -> Maybe b -> Either a b
maybeToRight a = maybe (Left a) Right
