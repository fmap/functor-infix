{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Functor.Infix.TH
( declareInfixFmapForFunctorCompositionOfDegree
, declareFlippedInfixFmapForFunctorCompositionOfDegree
, declareInfixFmapN
, declareInfixPamfN
) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (replicateM)
import Language.Haskell.TH (Q, Exp(..), Type(..), Dec(..), Pat(..), TyVarBndr(..), Pred(..), Body(..), newName, mkName, Fixity(..), FixityDirection(..))

(~>) :: Type -> Type -> Type
x ~> y = AppT ArrowT x `AppT` y
infixr 0 ~>

fmapTypeOfDegree :: Int -> Q Type
fmapTypeOfDegree n = do
  names@(a:b:fs) <- (mkName "a":) <$> (mkName "b":) <$> replicateM n (newName "f")
  let variables = map PlainTV names
#if MIN_VERSION_template_haskell(2,10,0)
      constraints = AppT (ConT $ mkName "Functor") . VarT <$> fs
#else
      constraints = map (ClassP (mkName "Functor") . pure . VarT) fs
#endif
      wrap hask = foldr AppT (VarT hask) $ map VarT fs
      type_ = (VarT a ~> VarT b) ~> wrap a ~> wrap b
  pure $ ForallT variables constraints type_

fmapExpressionOfDegree :: Int -> Q Exp
fmapExpressionOfDegree n = do
  (id_, fmap_) <- (,) <$> [|id|] <*> [|fmap|]
  pure $ foldr (AppE . AppE fmap_) id_ (replicate n fmap_)

declareInfixWithDegree :: (Int -> Q Exp) -> (Int -> Q Type) -> Fixity -> Char -> (Int -> Q [Dec])
declareInfixWithDegree expressionOfDegree typeOfDegree fixity symbol n = do
  let name = mkName $ "<" ++ replicate n symbol ++ ">"
  (expression, type_) <- (,) <$> expressionOfDegree n <*> typeOfDegree n
  pure $ SigD name type_
       : ValD (VarP name) (NormalB expression) []
       : InfixD fixity name
       : []

declareInfixFmapForFunctorCompositionOfDegree :: Int -> Q [Dec]
declareInfixFmapForFunctorCompositionOfDegree = declareInfixWithDegree fmapExpressionOfDegree fmapTypeOfDegree (Fixity 4 InfixL) '$'

pattern x :> y = AppT ArrowT x `AppT` y

(<$$>) :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap fmap fmap
infixl 1 <$$>

declareFlippedInfixFmapForFunctorCompositionOfDegree :: Int -> Q [Dec]
declareFlippedInfixFmapForFunctorCompositionOfDegree = do
  let flipExpression = AppE $ VarE (mkName "flip")
      flipType (ForallT variables constraints (a :> (b :> c))) = ForallT variables constraints (b ~> (a ~> c))
      flipType _ = error "The impossible happened!"
  declareInfixWithDegree (flipExpression <$$> fmapExpressionOfDegree) (flipType <$$> fmapTypeOfDegree) (Fixity 1 InfixL) '&'

{-# DEPRECATED declareInfixFmapN "Use 'declareInfixFmapForFunctorCompositionOfDegree' and/or reconsider your life choices." #-}
declareInfixFmapN :: Int -> Q [Dec]
declareInfixFmapN = declareInfixFmapForFunctorCompositionOfDegree

{-# DEPRECATED declareInfixPamfN "Use 'declareFlippedInfixFmapForFunctorCompositionOfDegree' and/or reconsider your life choices." #-}
declareInfixPamfN :: Int -> Q [Dec]
declareInfixPamfN = declareFlippedInfixFmapForFunctorCompositionOfDegree
