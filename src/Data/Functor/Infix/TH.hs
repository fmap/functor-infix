{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

module Data.Functor.Infix.TH (
  declareInfixFmapN
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Language.Haskell.TH (Q, Exp(..), Type(..), Dec(..), Pat(..), TyVarBndr(..), Pred(..), Body(..), newName, mkName)

declareInfixFmapN :: Int -> Q [Dec]
declareInfixFmapN n = do
  let name = mkName $ "<" ++ replicate n '$' ++ ">"
  (exp, typ) <- (,) <$> fmapExpN n <*> fmapTypeN n
  return [SigD name typ, ValD (VarP name) (NormalB exp) []] -- + Fixity?

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
