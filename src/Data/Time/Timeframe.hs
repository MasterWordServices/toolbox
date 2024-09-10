{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module       : Data.Time.Timeframe
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Intervals of time, that may by unbounded in either direction.
module Data.Time.Timeframe (
  -- * Frame extremities

  -- ** 'FrameStart'
  FrameStart (..),
  pattern BigBang,
  pattern FixedStart,

  -- ** 'FrameEnd'
  FrameEnd (..),
  pattern HeatDeath,
  pattern FixedEnd,

  -- ** Comparing opposite extremities
  compareStartEnd,
  minStartEnd,
  maxStartEnd,

  -- * The 'Timeframe' type
  Timeframe (..),
  timeframe,
  tfDuration,
  isDuring,
  isFinite,
  union,
  intersect,
  difference,
  unions,

  -- * 'Coverage'
  Coverage (..),
  cover,
  coverageDuration,
) where

import Control.Monad (liftM2)
import Data.List (foldl', sort, sortOn)
import Data.Ord.Toolbox (Down (..), maxOn, minOn)
import Data.Time

-- | The earliest point of a 'Timeframe'.
--
--   Pattern synonyms have been provided for simplicity and concision:
--
-- > BigBang <==> FrameStart Nothing
-- > FixedStart lt <==> FrameStart (Just lt)
newtype FrameStart = FrameStart {getFrameStart :: Maybe LocalTime} deriving (Eq, Ord, Show)

-- | The value representing an open-ended past for a 'FrameStart'.
pattern BigBang :: FrameStart
pattern BigBang = FrameStart Nothing

-- | The fixed time representing the start of a 'Timeframe'.
pattern FixedStart :: LocalTime -> FrameStart
pattern FixedStart lt = FrameStart (Just lt)

{-# COMPLETE BigBang, FixedStart #-}

-- | The latest point of a 'Timeframe'.
--
--   Pattern synonyms have been provided for simplicity and concision:
--
-- > HeatDeath <==> FrameEnd Nothing
-- > FixedEnd lt <==> FrameEnd (Just lt)
newtype FrameEnd = FrameEnd {getFrameEnd :: Maybe LocalTime} deriving (Eq, Show)

-- | The value representing an open-ended future for a 'FrameEnd'.
pattern HeatDeath :: FrameEnd
pattern HeatDeath = FrameEnd Nothing

-- | The fixed time representing the end of a 'Timeframe'.
pattern FixedEnd :: LocalTime -> FrameEnd
pattern FixedEnd lt = FrameEnd (Just lt)

{-# COMPLETE HeatDeath, FixedEnd #-}

instance Ord FrameEnd where
  HeatDeath `compare` HeatDeath = EQ
  HeatDeath `compare` _ = GT
  _ `compare` HeatDeath = LT
  FixedEnd a `compare` FixedEnd b = a `compare` b

-- | Compare opposite frame extremities without losing their respective ordering properties.
compareStartEnd :: FrameStart -> FrameEnd -> Ordering
compareStartEnd BigBang _ = LT
compareStartEnd _ HeatDeath = LT
compareStartEnd (FixedStart s) (FixedEnd e) = compare s e

-- | Get the minimum of two opposite frame extremities, where 'Nothing' represents 'BigBang'.
minStartEnd :: FrameStart -> FrameEnd -> Maybe LocalTime
minStartEnd s e = case compareStartEnd s e of
  GT -> getFrameEnd e
  _ -> getFrameStart s

-- | Get the maximum of two opposite frame extremities, where 'Nothing' represents 'HeatDeath'.
maxStartEnd :: FrameStart -> FrameEnd -> Maybe LocalTime
maxStartEnd s e = case compareStartEnd s e of
  GT -> getFrameStart s
  _ -> getFrameEnd e

-- | A time interval, potentially unbounded in either direction.
data Timeframe = Timeframe
  { tfStart :: FrameStart
  , tfEnd :: FrameEnd
  }
  deriving (Eq, Ord, Show)

-- | Create a 'Timeframe' from two 'LocalTime' values.
timeframe :: LocalTime -> LocalTime -> Timeframe
timeframe s e = Timeframe (FixedStart $ min s e) (FixedEnd $ max s e)

-- | Get the duration of a 'Timeframe'. Unbounded lengths of time are represented by 'Nothing'.
tfDuration :: Timeframe -> Maybe NominalDiffTime
tfDuration Timeframe{..} = diffLocalTime <$> getFrameEnd tfEnd <*> getFrameStart tfStart

-- | Is the given 'LocalTime' within the 'Timeframe' interval?
isDuring :: LocalTime -> Timeframe -> Bool
isDuring lt Timeframe{..} = tfStart <= FixedStart lt && FixedEnd lt <= tfEnd

-- | Is the 'Timeframe' interval bounded in both directions?
isFinite :: Timeframe -> Bool
isFinite (Timeframe (FixedStart _) (FixedEnd _)) = True
isFinite _ = False

-- | The set union of two timeframes viewed as intervals.
--   Non-overlapping timeframes will result in a 'Left' value; otherwise they will
--   be combined into a single 'Timeframe'.
union :: Timeframe -> Timeframe -> Either (Timeframe, Timeframe) Timeframe
union (Timeframe BigBang HeatDeath) _ = Right $ Timeframe BigBang HeatDeath
union _ (Timeframe BigBang HeatDeath) = Right $ Timeframe BigBang HeatDeath
union tf1@(Timeframe s1 e1) tf2@(Timeframe s2 e2) =
  case tf1 `intersect` tf2 of
    Nothing -> Left (minOn tfStart tf1 tf2, maxOn tfStart tf1 tf2)
    Just _ -> Right $ Timeframe (min s1 s2) (max e1 e2)

-- | The set intersection of two timeframes viewed as intervals.
intersect :: Timeframe -> Timeframe -> Maybe Timeframe
intersect (Timeframe s1 e1) (Timeframe s2 e2) =
  if s2 `compareStartEnd` e1 /= GT && s1 `compareStartEnd` e2 /= GT
    then Just $ Timeframe (max s1 s2) (min e1 e2)
    else Nothing

-- | The set difference of two timeframes viewed as intervals.
--
-- Depending on the overlap, we could end up with two separate blocks:
--
-- @
--       \<-------------------->
--           `difference`
--           s2|-------|e2
--
--
-- Left (\<-----|e1 , s2|------>)
-- @
--
-- Or, we could have a single block, that may or may not be shorter:
--
-- @
--       s1|----------------------|e1
--               `difference`
--            s2|-------------------|e2
--
--
-- Right s1|----|e1
-- @
--
-- Or still, we could have 'Nothing' left:
--
-- @
--           s1|----|e1
--         `difference`
--      s2|-------------->
--
--
-- Right Nothing
-- @
difference :: Timeframe -> Timeframe -> Either (Timeframe, Timeframe) (Maybe Timeframe)
difference tf1@(Timeframe s1 e1) tf2@(Timeframe s2 e2) =
  case tf1 `intersect` tf2 of
    Nothing -> Right $ Just tf1
    Just (Timeframe si ei) ->
      if si <= s1 && e1 <= ei
        then Right Nothing
        else Left (Timeframe s1 (FrameEnd $ minStartEnd si e2), Timeframe (FrameStart $ maxStartEnd s2 ei) e1)

-- | Combine all the 'Timeframe's into the largest possible blocks.
unions :: [Timeframe] -> [Timeframe]
unions = sort . unions' . sortOn Down . unions' . sort
 where
  unions' :: [Timeframe] -> [Timeframe]
  unions' (x : y : xs) = case x `union` y of
    Right tf -> tf : unions' xs
    Left (tf1, tf2) -> tf1 : unions' (tf2 : xs)
  unions' (x : xs) = x : unions' xs
  unions' [] = []

-- | A 'Coverage' is a list of timeframes that forms a monoid under 'unions'.
newtype Coverage = Coverage {getCoverage :: [Timeframe]} deriving (Eq, Ord, Show)

instance Semigroup Coverage where
  Coverage tfs1 <> Coverage tfs2 = Coverage . unions $ tfs1 <> tfs2

instance Monoid Coverage where mempty = Coverage []

-- | Compute the coverage of a single 'Timeframe'.
cover :: Timeframe -> Coverage
cover tf = Coverage [tf]

-- | Compute the total duration of a 'Coverage'.
coverageDuration :: Coverage -> Maybe NominalDiffTime
coverageDuration = foldl' (liftM2 (+)) (Just 0) . map tfDuration . getCoverage
