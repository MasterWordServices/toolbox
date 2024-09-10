{-# LANGUAGE ViewPatterns #-}

-- |
-- Module       : Data.Time.Calendar.Toolbox
-- Copyright    : (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Data.Time.Calendar'.
--
-- This module re-exports the above module, so modules need only import 'Data.Time.Calendar.Toolbox'.
module Data.Time.Calendar.Toolbox (
  -- * Convenience functions
  firstOfMonth,
  lastOfMonth,
  firstOfYear,
  lastOfYear,
  monthAgo,
  monthFromNow,

  -- * Re-exports
  module Data.Time.Calendar,
) where

import Data.Time.Calendar

-- | Get the first day of the given 'Day'\'s month.
firstOfMonth :: Day -> Day
firstOfMonth (toGregorian -> (y, m, _)) = fromGregorian y m 1

-- | Get the last day of the given 'Day'\'s month.
lastOfMonth :: Day -> Day
lastOfMonth (toGregorian -> (y, m, _)) = fromGregorian y m $ gregorianMonthLength y m

-- | Get the first day of the given 'Day'\'s year.
firstOfYear :: Day -> Day
firstOfYear (toGregorian -> (y, _, _)) = fromGregorian y 1 1

-- | Get the last day of the given 'Day'\'s year.
lastOfYear :: Day -> Day
lastOfYear (toGregorian -> (y, _, _)) = fromGregorian y 12 31

-- | Synonym for @'addGregorianMonthsClip' (-1)@.
--
-- > monthAgo (read "1993-09-01") == read "1993-08-01"
monthAgo :: Day -> Day
monthAgo = addGregorianMonthsClip (-1)

-- | Synonym for @'addGregorianMonthsClip' 1@.
--
-- > monthFromNow (read "1993-08-31") == read "1993-09-30"
monthFromNow :: Day -> Day
monthFromNow = addGregorianMonthsClip 1
