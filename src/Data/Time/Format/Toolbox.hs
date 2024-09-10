-- |
-- Module       : Data.Time.Format.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Data.Time.Format'.
--
-- This module re-exports the above module, so modules need only import 'Data.Time.Format.Toolbox'.
module Data.Time.Format.Toolbox (
  -- * Formatting
  mdy,
  ymd,
  shortMonthDayYear,
  longMonthDayYear,
  time24,
  time24s,
  time24ps,
  time12H,
  time12h,
  time12Hs,
  time12hs,
  ymd24,
  ymd24s,
  ymd24ps,
  ymd12H,
  ymd12h,
  ymd12Hs,
  parseMdy12hs,
  parseYmd12Hs,
  ymd12hs,
  mdy24,
  mdy24s,
  mdy24ps,
  mdy12H,
  mdy12h,
  mdy12Hs,
  mdy12hs,

  -- * Parsing
  parseMdy,
  parseYmd,
  parseShortMonthDayYear,
  parseLongMonthDayYear,
  parseTime24,
  parseTime24s,
  parseTime24ps,
  parseTime12H,
  parseTime12h,
  parseTime12Hs,
  parseTime12hs,
  parseYmd24,
  parseYmd24s,
  parseYmd24ps,
  parseYmd12H,
  parseYmd12h,
  parseYmd12hs,
  parseMdy24,
  parseMdy24s,
  parseMdy24ps,
  parseMdy12H,
  parseMdy12h,
  parseMdy12Hs,

  -- * Re-exports
  module Data.Time.Format,
) where

import Data.Function.Toolbox ((.:))
import Data.String (IsString (..))
import Data.Time
import Data.Time.Format

-- | Format according to the 'defaultTimeLocale'.
fmtdef :: (FormatTime t, IsString a) => String -> t -> a
fmtdef = fromString .: formatTime defaultTimeLocale

-- | Format as @MM\/DD\/YYYY@.
mdy :: (FormatTime t, IsString a) => t -> a
mdy = fmtdef "%m/%d/%Y"

-- | Format as @YYYY-MM-DD@.
ymd :: (FormatTime t, IsString a) => t -> a
ymd = fmtdef "%F"

-- | Format as @M DD YYYY@ where @M@ is the abbreviated name of the month
--   and @DD@ is the day of the month zero-padded to two digits.
--
-- > shortMonthDayYear (read "1993-09-01" :: Day) == "Sep 01 1993"
shortMonthDayYear :: (FormatTime t, IsString a) => t -> a
shortMonthDayYear = fmtdef "%b %0e %Y"

-- | Format as @M DD YYYY@ where @M@ is the full name of the month
--   and @DD@ is the unpadded day of the month.
--
-- > longMonthDayYear (read "1993-09-01" :: Day) == "September 1, 1993"
longMonthDayYear :: (FormatTime t, IsString a) => t -> a
longMonthDayYear = fmtdef "%B %-e, %Y"

-- | Format as @hh:mm@, where @hh@ is in the range @[00 .. 23]@.
--
-- > time24 (TimeOfDay 15 34 56) == "15:34"
time24 :: (FormatTime t, IsString a) => t -> a
time24 = fmtdef "%R"

-- | Format as @hh:mm:ss@, where @hh@ is in the range @[00 .. 23]@.
--
-- > time24s (TimeOfDay 15 34 56) == "15:34:56"
time24s :: (FormatTime t, IsString a) => t -> a
time24s = fmtdef "%T"

-- | Format as @hh:mm:ss(.pppppppppppp)@, where @hh@ is in the range @[00 .. 23]@.
--
-- > time24ps (TimeOfDay 15 34 56.3894324564719999) == "15:34:56.389432456471"
time24ps :: (FormatTime t, IsString a) => t -> a
time24ps = fmtdef "%T%Q"

-- | Format as @hh:mm (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
time12H :: (FormatTime t, IsString a) => t -> a
time12H = fmtdef "%I:%M %p"

-- | Format as @hh:mm (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
time12h :: (FormatTime t, IsString a) => t -> a
time12h = fmtdef "%I:%M %P"

-- | Format as @hh:mm:ss (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
time12Hs :: (FormatTime t, IsString a) => t -> a
time12Hs = fmtdef "%I:%M:%S %p"

-- | Format as @hh:mm:ss (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
time12hs :: (FormatTime t, IsString a) => t -> a
time12hs = fmtdef "%I:%M:%S %P"

-- | Format as @YYYY-MM-DD hh:mm@, where @hh@ is in the range @[00 .. 23]@.
ymd24 :: (FormatTime t, IsString a) => t -> a
ymd24 = fmtdef "%F %R"

-- | Format as @YYYY-MM-DD hh:mm:ss@, where @hh@ is in the range @[00 .. 23]@.
ymd24s :: (FormatTime t, IsString a) => t -> a
ymd24s = fmtdef "%F %T"

-- | Format as @YYYY-MM-DD hh:mm:ss@, where @hh@ is in the range @[00 .. 23]@.
ymd24ps :: (FormatTime t, IsString a) => t -> a
ymd24ps = fmtdef "%F %T%Q"

-- | Format as @YYYY-MM-DD hh:mm (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
ymd12H :: (FormatTime t, IsString a) => t -> a
ymd12H = fmtdef "%F %I:%M %p"

-- | Format as @YYYY-MM-DD hh:mm (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
ymd12h :: (FormatTime t, IsString a) => t -> a
ymd12h = fmtdef "%F %I:%M %P"

-- | Format as @YYYY-MM-DD hh:mm:ss (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
ymd12Hs :: (FormatTime t, IsString a) => t -> a
ymd12Hs = fmtdef "%F %I:%M:%S %p"

-- | Format as @YYYY-MM-DD hh:mm:ss (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
ymd12hs :: (FormatTime t, IsString a) => t -> a
ymd12hs = fmtdef "%F %I:%M:%S %P"

-- | Format as @MM/DD/YYYY hh:mm@, where @hh@ is in the range @[00 .. 23]@.
mdy24 :: (FormatTime t, IsString a) => t -> a
mdy24 = fmtdef "%m/%d/%Y %R"

-- | Format as @MM/DD/YYYY hh:mm:ss@, where @hh@ is in the range @[00 .. 23]@.
mdy24s :: (FormatTime t, IsString a) => t -> a
mdy24s = fmtdef "%m/%d/%Y %T"

-- | Format as @MM/DD/YYYY hh:mm:ss@, where @hh@ is in the range @[00 .. 23]@.
mdy24ps :: (FormatTime t, IsString a) => t -> a
mdy24ps = fmtdef "%m/%d/%Y %T%Q"

-- | Format as @MM/DD/YYYY hh:mm (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
mdy12H :: (FormatTime t, IsString a) => t -> a
mdy12H = fmtdef "%m/%d/%Y %I:%M %p"

-- | Format as @MM/DD/YYYY hh:mm (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
mdy12h :: (FormatTime t, IsString a) => t -> a
mdy12h = fmtdef "%m/%d/%Y %I:%M %P"

-- | Format as @MM/DD/YYYY hh:mm:ss (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
mdy12Hs :: (FormatTime t, IsString a) => t -> a
mdy12Hs = fmtdef "%m/%d/%Y %I:%M:%S %p"

-- | Format as @MM/DD/YYYY hh:mm:ss (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
mdy12hs :: (FormatTime t, IsString a) => t -> a
mdy12hs = fmtdef "%m/%d/%Y %I:%M:%S %P"

-- | Parse according to the 'defaultTimeLocale'.
prsdef :: (ParseTime t) => String -> String -> Maybe t
prsdef = parseTimeM False defaultTimeLocale

-- | Parse as @MM\/DD\/YYYY@.
parseMdy :: (ParseTime t) => String -> Maybe t
parseMdy = prsdef "%m/%d/%Y"

-- | Parse as @YYYY-MM-DD@.
parseYmd :: (ParseTime t) => String -> Maybe t
parseYmd = prsdef "%F"

-- | Parse as @M DD YYYY@ where @M@ is the abbreviated name of the month
--   and @DD@ is the day of the month zero-padded to two digits.
--
-- > shortMonthDayYear (read "1993-09-01" :: Day) == "Sep 01 1993"
parseShortMonthDayYear :: (ParseTime t) => String -> Maybe t
parseShortMonthDayYear = prsdef "%b %0e %Y"

-- | Parse as @M DD YYYY@ where @M@ is the full name of the month
--   and @DD@ is the unpadded day of the month.
--
-- > longMonthDayYear (read "1993-09-01" :: Day) == "September 1, 1993"
parseLongMonthDayYear :: (ParseTime t) => String -> Maybe t
parseLongMonthDayYear = prsdef "%B %-e, %Y"

-- | Parse as @hh:mm@, where @hh@ is in the range @[00 .. 23]@.
--
-- > time24 (TimeOfDay 15 34 56) == "15:34"
parseTime24 :: (ParseTime t) => String -> Maybe t
parseTime24 = prsdef "%R"

-- | Parse as @hh:mm:ss@, where @hh@ is in the range @[00 .. 23]@.
--
-- > time24s (TimeOfDay 15 34 56) == "15:34:56"
parseTime24s :: (ParseTime t) => String -> Maybe t
parseTime24s = prsdef "%T"

-- | Parse as @hh:mm:ss(.pppppppppppp)@, where @hh@ is in the range @[00 .. 23]@.
--
-- > time24ps (TimeOfDay 15 34 56.3894324564719999) == "15:34:56.389432456471"
parseTime24ps :: (ParseTime t) => String -> Maybe t
parseTime24ps = prsdef "%T%Q"

-- | Parse as @hh:mm (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
parseTime12H :: (ParseTime t) => String -> Maybe t
parseTime12H = prsdef "%I:%M %p"

-- | Parse as @hh:mm (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
parseTime12h :: (ParseTime t) => String -> Maybe t
parseTime12h = prsdef "%I:%M %P"

-- | Parse as @hh:mm:ss (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
parseTime12Hs :: (ParseTime t) => String -> Maybe t
parseTime12Hs = prsdef "%I:%M:%S %p"

-- | Parse as @hh:mm:ss (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
parseTime12hs :: (ParseTime t) => String -> Maybe t
parseTime12hs = prsdef "%I:%M:%S %P"

-- | Parse as @YYYY-MM-DD hh:mm@, where @hh@ is in the range @[00 .. 23]@.
parseYmd24 :: (ParseTime t) => String -> Maybe t
parseYmd24 = prsdef "%F %R"

-- | Parse as @YYYY-MM-DD hh:mm:ss@, where @hh@ is in the range @[00 .. 23]@.
parseYmd24s :: (ParseTime t) => String -> Maybe t
parseYmd24s = prsdef "%F %T"

-- | Parse as @YYYY-MM-DD hh:mm:ss@, where @hh@ is in the range @[00 .. 23]@.
parseYmd24ps :: (ParseTime t) => String -> Maybe t
parseYmd24ps = prsdef "%F %T%Q"

-- | Parse as @YYYY-MM-DD hh:mm (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
parseYmd12H :: (ParseTime t) => String -> Maybe t
parseYmd12H = prsdef "%F %I:%M %p"

-- | Parse as @YYYY-MM-DD hh:mm (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
parseYmd12h :: (ParseTime t) => String -> Maybe t
parseYmd12h = prsdef "%F %I:%M %P"

-- | Parse as @YYYY-MM-DD hh:mm:ss (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
parseYmd12Hs :: (ParseTime t) => String -> Maybe t
parseYmd12Hs = prsdef "%F %I:%M:%S %p"

-- | Parse as @YYYY-MM-DD hh:mm:ss (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
parseYmd12hs :: (ParseTime t) => String -> Maybe t
parseYmd12hs = prsdef "%F %I:%M:%S %P"

-- | Parse as @MM/DD/YYYY hh:mm@, where @hh@ is in the range @[00 .. 23]@.
parseMdy24 :: (ParseTime t) => String -> Maybe t
parseMdy24 = prsdef "%m/%d/%Y %R"

-- | Parse as @MM/DD/YYYY hh:mm:ss@, where @hh@ is in the range @[00 .. 23]@.
parseMdy24s :: (ParseTime t) => String -> Maybe t
parseMdy24s = prsdef "%m/%d/%Y %T"

-- | Parse as @MM/DD/YYYY hh:mm:ss@, where @hh@ is in the range @[00 .. 23]@.
parseMdy24ps :: (ParseTime t) => String -> Maybe t
parseMdy24ps = prsdef "%m/%d/%Y %T%Q"

-- | Parse as @MM/DD/YYYY hh:mm (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
parseMdy12H :: (ParseTime t) => String -> Maybe t
parseMdy12H = prsdef "%m/%d/%Y %I:%M %p"

-- | Parse as @MM/DD/YYYY hh:mm (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
parseMdy12h :: (ParseTime t) => String -> Maybe t
parseMdy12h = prsdef "%m/%d/%Y %I:%M %P"

-- | Parse as @MM/DD/YYYY hh:mm:ss (AM|PM)@, where @hh@ is in the range @[01 .. 12]@.
parseMdy12Hs :: (ParseTime t) => String -> Maybe t
parseMdy12Hs = prsdef "%m/%d/%Y %I:%M:%S %p"

-- | Parse as @MM/DD/YYYY hh:mm:ss (am|pm)@, where @hh@ is in the range @[01 .. 12]@.
parseMdy12hs :: (ParseTime t) => String -> Maybe t
parseMdy12hs = prsdef "%m/%d/%Y %I:%M:%S %P"
