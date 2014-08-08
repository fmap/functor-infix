{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

module Data.Functor.Infix.TH (
  declareInfixFmapN,
  declareInfixPamfN
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Language.Haskell.TH (Q, Exp(..), Type(..), Dec(..), Pat(..), TyVarBndr(..), Pred(..), Body(..), newName, mkName, Fixity(..), FixityDirection(..))

declareInfixFmapN :: Int -> Q [Dec]
declareInfixFmapN = declareInfixN fmapExpN fmapTypeN (Fixity 4 InfixL) '$'

fmapExpN :: Int -> Q Exp
fmapExpN n = do
  (idt, fmp) <- (,) <$> [|id|] <*> [|fmap|]
  return $ foldr (AppE . AppE fmp) idt (replicate n fmp)

fmapTypeN :: Int -> Q Type
fmapTypeN n = do
  (varsAB, varsFu) <- ([mkName "a", mkName "b"],) <$> replicateM n (newName "f")
  let vrs = PlainTV <$> varsAB ++ varsFu
      cns = ClassP (mkName "Functor") . return . VarT <$> varsFu
      wrp = \n -> foldr AppT (VarT n) $ VarT <$> varsFu
      typ = AppT (AppT ArrowT (AppT (AppT ArrowT (VarT $ varsAB!!0)) (VarT $ varsAB!!1))) (AppT (AppT ArrowT . wrp $ varsAB!!0) (wrp $ varsAB!!1))
  return $ ForallT vrs cns typ

declareInfixPamfN :: Int -> Q [Dec]
declareInfixPamfN = declareInfixN pamfExpN pamfTypeN (Fixity 1 InfixL) '&'

pamfExpN :: Int -> Q Exp
pamfExpN n = [|flip|] >>= \flip -> AppE flip <$> fmapExpN n

pamfTypeN :: Int -> Q Type
pamfTypeN n = fmapTypeN n >>= \(ForallT crs wrp (AppT (AppT ArrowT ab) (AppT (AppT ArrowT x) y))) ->
  return $ ForallT crs wrp (AppT (AppT ArrowT x) (AppT (AppT ArrowT ab) y))

declareInfixN :: (Int -> Q Exp) -> (Int -> Q Type) -> Fixity -> Char -> Int -> Q [Dec]
declareInfixN expN typN fixity chr n = do
  let name = mkName $ "<" ++ replicate n chr ++ ">"
  (exp, typ) <- (,) <$> expN n <*> typN n
  return [SigD name typ, ValD (VarP name) (NormalB exp) [], InfixD fixity name]
