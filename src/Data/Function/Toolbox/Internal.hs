module Data.Function.Toolbox.Internal where

import Control.Monad (forM, replicateM)
import Data.Foldable (foldl')
import Language.Haskell.TH (
  Body (NormalB),
  Clause (Clause),
  Dec (FunD),
  Exp (..),
  Pat (..),
  Q,
  mkName,
  newName,
 )

curryN, uncurryN :: (Integral n) => n -> Q Exp
curryN n = do
  f <- newName "f"
  xs <- replicateM (fromIntegral n) (newName "x")
  pure $ LamE (map VarP (f : xs)) $ AppE (VarE f) $ TupE (map (Just . VarE) xs)
uncurryN n = do
  f <- newName "f"
  xs <- replicateM (fromIntegral n) (newName "x")
  pure $ LamE [VarP f, TupP (map VarP xs)] $ foldl' ((. VarE) . AppE) (VarE f) xs

mkCurryN, mkUncurryN :: Int -> Q [Dec]
mkCurryN n = forM [3 .. n] $ \i -> do
  f <- curryN i
  let name = mkName $ "curry" ++ show i
  pure $ FunD name [Clause [] (NormalB f) []]
mkUncurryN n = forM [3 .. n] $ \i -> do
  f <- uncurryN i
  let name = mkName $ "uncurry" ++ show i
  pure $ FunD name [Clause [] (NormalB f) []]
