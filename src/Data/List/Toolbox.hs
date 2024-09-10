{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- |
-- Module       : Data.List.Toolbox
-- Copyright    : (c) Melanie Brown 2021
-- License      : BSD3 (see the file LICENSE)
-- Maintainer   : brown.m@pm.me
--
-- Utility functions on top of 'Data.List'.
--
-- This module re-exports the above module, so modules need only import 'Data.List.Toolbox'.
module Data.List.Toolbox (
  -- * Basic combinators
  list,
  safeHead,
  safeLast,
  safeTail,
  safeInit,
#if !MIN_VERSION_base(4,19,0)
  unsnoc,
#endif
  tuple2,
  tuple3,
  tuple4,
  sublists,
#if !MIN_VERSION_base(4,19,0)
  (!?),
#endif
  enumerate,
  chopInfix,
  removed,
  replace,
  replaceFirst,
  splitOn,
  chunksOf,
  genericChunksOf,

  -- * Predicates over lists
  count,
  genericCount,
  takeEnd,
  genericTakeEnd,
  dropEnd,
  genericDropEnd,
  takeWhileEnd,
  spanEnd,
  breakEnd,

  -- * Predicates about lists
  anySame,
  anySameOrd,
  allSame,
  disjoint,
  disjointOrd,

  -- * Ordered lists
  merge,
  mergeBy,
  mergeOn,
  groupOn,
  groupSort,
  groupSortOn,
  intersectOn,
  unionOn,

  -- * Unique prefixes
  takeWhileUnique,
  dropWhileUnique,
  takeWhileUniqueOrd,
  dropWhileUniqueOrd,
  iterateWhileUnique,

  -- * Re-exports
  module Data.List,
) where

import Data.Bifunctor (first, second)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (foldMap'), foldlM, toList)
import Data.Function.Toolbox (using)
import Data.List
import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set

-- | A non-recursive eliminator for @[a]@.
--
-- > \xs -> list Nothing (const . Just) xs == listToMaybe xs
list :: b -> (a -> [a] -> b) -> [a] -> b
list n c lx = case lx of
  [] -> n
  (x : xs) -> c x xs

-- | Get the first element of a (possibly empty) list safely.
--   A convenient synonym for 'listToMaybe'.
safeHead :: [a] -> Maybe a
safeHead xs = listToMaybe xs

-- | Get the last element of a (possibly empty) list safely.
--
-- > safeLast [] == Nothing
-- > safeLast [2, 3, 4] == Just 4
safeLast :: [a] -> Maybe a
safeLast xs = foldl' (const Just) Nothing xs

-- | Get the tail of a (possibly empty) list safely.
--
-- > safeTail [2, 3, 4] == Just [3, 4]
-- > safeTail [] == Nothing
safeTail :: [a] -> Maybe [a]
safeTail xs = list Nothing (const Just) xs

-- | Get the 'init' of a (possibly empty) list safely.
--
-- > safeInit [2, 3, 4] == Just [2, 3]
-- > safeInit [] == Nothing
safeInit :: [a] -> Maybe [a]
safeInit xs = list Nothing (const $ Just . init) xs

#if !MIN_VERSION_base(4,19,0)
-- | Get the 'init' and 'last' of a (possibly empty) list safely.
--
-- > unsnoc [] == Nothing
-- > unsnoc [1, 2, 3] == Just ([1, 2], 3)
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc as = Just (init as, last as)
#endif

-- | The list of all ordered sublists.
--
-- >>> sublists [1, 5, 9, 13]
-- [[1,5,9,13],[1,5,9],[1,5,13],[1,5],[1,9,13],[1,9],[1,13],[1],[5,9,13],[5,9],[5,13],[5],[9,13],[9],[13],[]]
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x : xs) = let ys = sublists xs in map (x :) ys ++ ys

-- | Create a pair from the first two elements of a list, if they exist.
tuple2 :: [a] -> Maybe (a, a)
tuple2 (x : y : _) = Just (x, y)
tuple2 _ = Nothing

-- | Create a triple from the first three elements of a list, if they exist.
tuple3 :: [a] -> Maybe (a, a, a)
tuple3 (x : y : z : _) = Just (x, y, z)
tuple3 _ = Nothing

-- | Create a triple from the first three elements of a list, if they exist.
tuple4 :: [a] -> Maybe (a, a, a, a)
tuple4 (x : y : z : w : _) = Just (x, y, z, w)
tuple4 _ = Nothing

#if !MIN_VERSION_base(4,19,0)
-- | Get the value of a list at an index, if the list is long enough. Safe version of '(!!)'.
--
-- > ['a', 'b', 'c'] !? 2 == Just 'c'
-- > ['a', 'b', 'c'] !? 4 == Nothing
-- > \xs n -> n < 0 ==> xs !? n == Nothing
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
xs !? 0 = safeHead xs
(_ : xs) !? n = if n < 0 then Nothing else xs !? pred n
#endif

-- | Check if the list contains duplicates. /O(n^2)/.
--
-- > anySame [2, 3, 4, 3] == True
-- > anySame [2, 5, 6, 7] == False
anySame :: (Eq a) => [a] -> Bool
anySame xs = go [] xs
 where
  go :: (Eq a) => [a] -> [a] -> Bool
  go seen (y : ys) = y `elem` seen || go (y : seen) ys
  go _ [] = False

-- | /O(n log n)/. Check if the list contains duplicates efficiently.
--
-- > \xs -> anySame xs == anySameOrd xs
anySameOrd :: (Ord a) => [a] -> Bool
anySameOrd xs = go Set.empty xs
 where
  go :: (Ord a) => Set a -> [a] -> Bool
  go seen (y : ys) = y `Set.member` seen || go (Set.insert y seen) ys
  go _ [] = False

-- | /O(n)/. Check if all the elements of the list are equal.
--
-- > allSame [2, 2, 2] == True
-- > allSame [2, 3, 4] == False
allSame :: (Eq a) => [a] -> Bool
allSame xs = list True (all . (==)) xs

-- | Tests if the lists have no elements in common.
--
-- > disjoint "abc" "def" == True
-- > disjoint "" undefined == True
-- > disjoint undefined "" == undefined
-- > disjoint "abc" "cde" == False
disjoint :: (Eq a) => [a] -> [a] -> Bool
disjoint xs ys = null $ intersect xs ys

-- | Like 'disjoint', but with better algorithmic complexity.
disjointOrd :: (Ord a) => [a] -> [a] -> Bool
disjointOrd xs ys = (Set.disjoint `using` Set.fromList) xs ys

-- | An ordered list of values of a 'Bounded' 'Enum' type.
enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

-- | A version of 'merge' that accepts a user-defined comparison function.
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] ys = ys
mergeBy _ xs [] = xs
mergeBy f (x : xs) (y : ys) = case f x y of
  GT -> y : mergeBy f (x : xs) ys
  _ -> x : mergeBy f xs (y : ys)

-- | Combine two lists, preserving order. Assumes both input lists have been sorted.
--
-- > merge [2, 3, 6] [1, 4, 7] == [1, 2, 3, 4, 6, 7]
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs ys = mergeBy compare xs ys

-- | A version of 'mergeBy' that accepts a valuation function instead of a comparison.
mergeOn :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
mergeOn f xs ys = mergeBy (compare `using` f) xs ys

-- | Find the number of elements that satisfy the predicate.
--
-- > count p == length . filter p
count :: (a -> Bool) -> [a] -> Int
count p xs = coerce $ foldMap' (coerce (fromEnum . p) `asTypeOf` (Sum . fromEnum . p)) xs

-- | A version of 'count' that is polymorphic in the return type.
genericCount :: (Integral n) => (a -> Bool) -> [a] -> n
genericCount p xs = fromIntegral $ count p xs

-- | A version of 'take' that keeps values from the end of the list.
--
-- > takeEnd 2 [1, 2, 3] == [2, 3]
-- > \xs i -> i >= 0 ==> takeEnd i xs == drop (length xs - i) xs
takeEnd :: Int -> [a] -> [a]
takeEnd n xs = if n <= 0 then [] else go xs (drop n xs)
 where
  go :: [a] -> [a] -> [a]
  go (_ : as) (_ : bs) = go as bs
  go as _ = as

-- | A version of 'drop' that removes values from the end of the list.
--
-- > dropEnd 2 [1, 2, 3] == [1]
-- > \xs i -> i >= 0 ==> dropEnd i xs == take (length xs - i) xs
dropEnd :: Int -> [a] -> [a]
dropEnd n xs = if n <= 0 then xs else zipWith const xs (drop n xs)

-- | A version of 'takeWhile' that keeps values from the end of the list.
--
-- > takeWhileEnd odd [2, 4, 1, 3, 5] == [1, 3, 5]
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p xs = reverse . takeWhile p $ reverse xs

-- | A version of 'takeEnd' where the parameneter can be of any 'Integral' type.
genericTakeEnd :: (Integral n) => n -> [a] -> [a]
genericTakeEnd xs = takeEnd $ fromIntegral xs

-- | A version of 'dropEnd' where the parameter can be of any 'Integral' type.
genericDropEnd :: Int -> [a] -> [a]
genericDropEnd xs = dropEnd $ fromIntegral xs

-- | A version of 'span' that runs from the end of the list.
--
-- > spanEnd odd [1, 7, 4, 3, 5] == ([3, 5], [1, 7, 4])
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p xs = (takeWhileEnd p xs, dropWhileEnd p xs)

-- | A version of 'break' that runs from the end of the list.
--
-- > breakEnd odd [1, 7, 4, 3, 5] == ([1, 7, 4], [3, 5])
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd p xs = (dropWhileEnd p xs, takeWhileEnd p xs)

-- | Like 'removed', but keeps the search string as a prefix.
--
-- > chopInfix (fromList "c") "abcde" == ("ab", "cde")
chopInfix :: (Eq a) => NonEmpty a -> [a] -> ([a], [a])
chopInfix _ [] = ([], [])
chopInfix as lx@(x : xs) =
  if toList as `isPrefixOf` lx
    then ([], lx)
    else first (x :) $ chopInfix as xs

-- | @'removed' as xs@ removes the first occurrence of @as@ from @xs@ by
--   returning the surrounding prefix and suffix.
--
-- > removed "" "abc" == ("", "abc")
-- > removed "c" "abcde" == ("ab", "de")
removed :: (Eq a) => [a] -> [a] -> ([a], [a])
removed [] xs = ([], xs)
removed as xs = second (drop (length as)) $ chopInfix (fromList as) xs

-- | A version of 'replace' that only substitutes the first occurrence.
--
-- > replaceFirst (fromList "z") "" "azbzc" == "abzc"
-- > replaceFirst (fromList "z") "xy" "azbzc" == "axybzc"
replaceFirst :: (Eq a) => NonEmpty a -> [a] -> [a] -> [a]
replaceFirst as bs xs = (\(pre, post) -> pre ++ bs ++ post) $ removed (toList as) xs

-- | A version of 'groupBy' that accepts a user-defined function on which to test equality.
--
-- > groupOn fst [(1, 3), (1, 4), (2, 4)] == [[(1, 3), (1, 4)], [(2, 4)]]
-- > groupOn snd [(1, 3), (1, 4), (2, 4)] == [[(1, 3)], [(1, 4), (2, 4)]]
groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f xs = groupBy ((==) `using` f) xs

-- | A composition of 'group' and 'sort'.
--
-- > groupSort [1, 3, 4, 3, 1] == [[1, 1], [3, 3], [4]]
groupSort :: (Ord a) => [a] -> [[a]]
groupSort xs = group $ sort xs

-- | A version of 'groupSort' that accepts a user-defined comparison function.
--
-- > groupSortOn fst [(1, 3), (2, 4), (1, 4)] == [[(1, 3), (1, 4)], [(2, 4)]]
-- > groupSortOn snd [(1, 3), (2, 4), (1, 4)] == [[(1, 3)], [(2, 4), (1, 4)]]
groupSortOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupSortOn f xs = map (map (snd . getFst)) . groupSort $ map (mkFst f) xs

-- | A version of 'intersectBy' that accepts a user-defined function on which to test equality.
--   Prefers elements from the first list.
--
-- > intersectOn abs [-1, -2] [2, 3] == [-2]
-- > intersectOn signum [-1, -2] [2, 3] == []
intersectOn :: (Eq b) => (a -> b) -> [a] -> [a] -> [a]
intersectOn f xs = intersectBy ((==) `using` f) xs

-- | A version of 'unionBy' that accepts a user-defined function on which to test equality.
--   Prefers elements from the first list.
--
-- > unionOn abs [-1, -2] [2, 3] == [-1, -2, 3]
-- > unionOn abs [-1] [2, 3] == [-1, 2, 3]
unionOn :: (Eq b) => (a -> b) -> [a] -> [a] -> [a]
unionOn f xs = unionBy ((==) `using` f) xs

-- | @'replace' as bs xs@ substitutes the sublist @as@ with @bs@ each place @as@ occurs in @xs@.
--   To replace only the first occurrence, use 'replaceFirst'.
--
-- > replace (fromList "c") "z" "abcdec" == "abzdez"
-- > replace (fromList "c") "a" "vwxyz" == "vwxyz"
replace :: (Eq a) => NonEmpty a -> [a] -> [a] -> [a]
replace _ _ [] = []
replace la bs lx@(x : xs) =
  if as `isPrefixOf` lx
    then bs ++ replace la bs (fromJust (stripPrefix as lx))
    else x : replace la bs xs
 where
  as = toList la

-- | A version of 'replace' where instead of conjoining the prefix and suffix,
--   they are kept as separate sublists.
--
-- > splitOn (fromList "z") "azbzc" == ["a","b","c"]
-- > splitOn (fromList "z") "azbzcz" == ["a","b","c",""]
-- > splitOn (fromList "z") "zazbzc" == ["","a","b","c"]
splitOn :: (Eq a) => NonEmpty a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn as lx =
  pre : case post of
    [] -> []
    _ -> splitOn as $ drop (length as) post
 where
  (pre, post) = chopInfix as lx

-- | Split a list into chunks of a given size.
--
-- > chunksOf 2 "abcde" == ["ab", "cd", "e"]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Keep elements from a list until a duplicate is seen.
--
-- > takeWhileUnique [1, 2, 3, 4, 3, 2, 1] == [1, 2, 3, 4]
takeWhileUnique :: (Eq a) => [a] -> [a]
takeWhileUnique xs = either reverse reverse $ foldlM unique [] xs
 where
  unique :: (Eq a) => [a] -> a -> Either [a] [a]
  unique acc x =
    if x `elem` acc
      then Left acc
      else Right $ x : acc

-- | Remove elements from a list until a duplicate is seen.
--
-- > dropWhileUnique [1, 2, 3, 4, 3, 2, 1] == [3, 2, 1]
dropWhileUnique :: (Eq a) => [a] -> [a]
dropWhileUnique xs = drop (length $ takeWhileUnique xs) xs

-- | Iterate a function until its result reoccurs.
--
-- > iterateWhileUnique (`div` 3) 9 == [9, 3, 1]
-- > iterateWhileUnique succ 0 == _|_
iterateWhileUnique :: (Eq a) => (a -> a) -> a -> [a]
iterateWhileUnique f xs = iterateWhileUnique' [] f xs
 where
  iterateWhileUnique' :: (Eq a) => [a] -> (a -> a) -> a -> [a]
  iterateWhileUnique' !acc g !x
    | x `elem` acc = acc
    | otherwise = iterateWhileUnique' (acc ++ [x]) g (g x)

-- | Like 'takeWhileUnique', but asymptotically faster due to the 'Ord' constraint.
takeWhileUniqueOrd :: (Eq a, Ord a) => [a] -> [a]
takeWhileUniqueOrd xs = either (reverse . fst) (reverse . fst) $ foldlM unique ([], Set.empty) xs
 where
  unique :: (Eq a, Ord a) => ([a], Set a) -> a -> Either ([a], Set a) ([a], Set a)
  unique (l, acc) x =
    if x `Set.member` acc
      then Left (l, acc)
      else Right (x : l, Set.insert x acc)

-- | Like 'dropWhileUnique', but asymptotically faster due to the 'Ord' constraint.
dropWhileUniqueOrd :: (Eq a, Ord a) => [a] -> [a]
dropWhileUniqueOrd xs = drop (length $ takeWhileUniqueOrd xs) xs

-- | A version of 'chunksOf' where the parameter can be of any 'Integral' type.
genericChunksOf :: (Integral n) => n -> [a] -> [[a]]
genericChunksOf n xs = chunksOf (fromIntegral n) xs

newtype Fst a b = Fst {getFst :: (b, a)}

instance Eq b => Eq (Fst a b) where (==) = (==) `using` (fst . getFst)

instance Ord b => Ord (Fst a b) where compare = compare `using` (fst . getFst)

mkFst :: (a -> b) -> a -> Fst a b
mkFst f x = Fst (f x, x)
