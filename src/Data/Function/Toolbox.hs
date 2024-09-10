{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- |
-- Module       : Data.Function.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Data.Function'.
--
-- This module re-exports the above module, so modules need only import 'Data.Function.Toolbox'.
module Data.Function.Toolbox (
  -- * Re-exports
  module Data.Function,

  -- * Extra utilities
  applyN,
  (.:),
  using,

  -- * More 'const' functions
  const2,
  const3_1,
  const3_2,
  const3_3,
  const4_1,
  const4_2,
  const4_3,
  const4_4,

  -- * More curries and uncurries
  curry3,
  curry4,
  curry5,
  curry6,
  curry7,
  curry8,
  curry9,
  curry10,
  curry11,
  curry12,
  curry13,
  curry14,
  curry15,
  curry16,
  curry17,
  curry18,
  curry19,
  curry20,
  uncurry3,
  uncurry4,
  uncurry5,
  uncurry6,
  uncurry7,
  uncurry8,
  uncurry9,
  uncurry10,
  uncurry11,
  uncurry12,
  uncurry13,
  uncurry14,
  uncurry15,
  uncurry16,
  uncurry17,
  uncurry18,
  uncurry19,
  uncurry20,
) where

import Data.Function
import Data.Function.Toolbox.Internal (mkCurryN, mkUncurryN)
import Data.Semigroup (Endo (..), stimesMonoid)

-- | Apply a function 'n' times.
--
-- > \x -> applyN 5 succ x == x + 5
applyN :: (Integral n) => n -> (a -> a) -> (a -> a)
applyN n = appEndo . stimesMonoid n . Endo

-- | Apply a single-argument function after a two-argument function.
--
-- > \x y -> (succ .: (+)) x y == (x + y) + 1
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .: f = (g .) . f

-- | Like 'on', but memoizes values to minimize necessary calculations.
using :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
cmp `using` f = \x -> let fx = f x in cmp fx . f

-- | A named @(\\_ x -> x)@.
const2 :: p1 -> p2 -> p2
const2 _ x = x

-- | Three-argument 'const' that keeps the first value.
const3_1 :: p1 -> p2 -> p3 -> p1
const3_1 a _ _ = a

-- | Three-argument 'const' that keeps the second value.
const3_2 :: p1 -> p2 -> p3 -> p2
const3_2 _ b _ = b

-- | Three-argument 'const' that keeps the third value.
const3_3 :: p1 -> p2 -> p3 -> p3
const3_3 _ _ c = c

-- | Four-argument 'const' that keeps the first value.
const4_1 :: p1 -> p2 -> p3 -> p4 -> p1
const4_1 a _ _ _ = a

-- | Four-argument 'const' that keeps the second value.
const4_2 :: p1 -> p2 -> p3 -> p4 -> p2
const4_2 _ b _ _ = b

-- | Four-argument 'const' that keeps the third value.
const4_3 :: p1 -> p2 -> p3 -> p4 -> p3
const4_3 _ _ c _ = c

-- | Four-argument 'const' that keeps the fourth value.
const4_4 :: p1 -> p2 -> p3 -> p4 -> p4
const4_4 _ _ _ d = d

$(mkCurryN 20)
$(mkUncurryN 20)
