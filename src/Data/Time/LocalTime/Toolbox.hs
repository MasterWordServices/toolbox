-- |
-- Module       : Data.Time.LocalTime.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Data.Time.LocalTime'.
--
-- This module re-exports the above module, so modules need only import 'Data.Time.LocalTime.Toolbox'.
module Data.Time.LocalTime.Toolbox (
  -- * Convenience functions
  getCurrentLocalTime,
  addSeconds,
  lastMinute,
  lastSecond,

  -- * Re-exports
  module Data.Time.LocalTime,
) where

import Data.Time.Calendar
import Data.Time.LocalTime

-- | Forget the time zone from 'getZonedTime'.
getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = zonedTimeToLocalTime <$> getZonedTime

-- | Convenience function around 'addLocalTime'.
addSeconds :: Int -> LocalTime -> LocalTime
addSeconds = addLocalTime . fromIntegral

-- | 23:59:00 on the given day.
lastMinute :: Day -> LocalTime
lastMinute day = addSeconds (-60) $ LocalTime (succ day) midnight

-- | 23:59:59 on the given day.
lastSecond :: Day -> LocalTime
lastSecond day = addSeconds (-1) $ LocalTime (succ day) midnight
