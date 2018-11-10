{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Functor.Infix.TH
( declareInfixFmapForFunctorCompositionOfDegree
, declareInfixFmapConstForFunctorCompositionOfDegree
, declareFlippedInfixFmapForFunctorCompositionOfDegree
, declareFlippedInfixFmapConstForFunctorCompositionOfDegree
, declareInfixFmapN
, declareInfixPamfN
) where

import Control.Applicative ((<$>), (<*>), liftA2, pure)
import Control.Monad (guard, replicateM)
import Language.Haskell.TH (Q, Exp(..), Type(..), Dec(..), Pat(..), TyVarBndr(..), Pred(..), Body(..), newName, mkName, Name, Fixity(..), FixityDirection(..))

(~>) :: Type -> Type -> Type
x ~> y = AppT ArrowT x `AppT` y
infixr 0 ~>

outMorphismOfDegree :: Name -> Name -> Int -> Q ([Name], [Type], Type)
outMorphismOfDegree a b n = do
  fs <- replicateM n (newName "f")
#if MIN_VERSION_template_haskell(2,10,0)
  let constraints = AppT (ConT $ mkName "Functor") . VarT <$> fs
#else
      constraints = map (ClassP (mkName "Functor") . pure . VarT) fs
#endif
      wrap hask = foldr AppT (VarT hask) $ map VarT fs
      type_ = wrap a ~> wrap b
  pure (fs, constraints, type_)

fmapTypeOfDegree :: Int -> Q Type
fmapTypeOfDegree n = do
  names@[a, b] <- mapM newName ["a", "b"]
  (fs, constraints, outType) <- outMorphismOfDegree a b n
  let variables = map PlainTV $ names ++ fs
      type_ = (VarT a ~> VarT b) ~> outType
  pure $ ForallT variables constraints type_

fmapConstTypeOfDegree :: Int -> Q Type
fmapConstTypeOfDegree n = do
  names@[a, b] <- mapM newName ["a", "b"]
  (fs, constraints, outType) <- outMorphismOfDegree b a n
  let variables = map PlainTV $ names ++ fs
      type_ = VarT a ~> outType
  pure $ ForallT variables constraints type_

fmapExpressionOfDegree :: Int -> Q Exp
fmapExpressionOfDegree n = do
  (id_, fmap_) <- (,) <$> [|id|] <*> [|fmap|]
  pure $ foldr (AppE . AppE fmap_) id_ (replicate n fmap_)

fmapConstExpressionOfDegree :: Int -> Q Exp
fmapConstExpressionOfDegree n = do
  [| \x -> $(fmapExpressionOfDegree n) (const x) |]

data LR = Both | L | R
  deriving (Eq)

declareInfixWithDegree :: (Int -> Q Exp) -> (Int -> Q Type) -> Fixity -> Char -> LR -> Int -> Q [Dec]
declareInfixWithDegree expressionOfDegree typeOfDegree fixity symbol lr n = do
  let name = mkName $ ("<" <* guard (lr /= R))
                   ++ replicate n symbol
                   ++ (">" <* guard (lr /= L))
  (expression, type_) <- (,) <$> expressionOfDegree n <*> typeOfDegree n
  pure $ SigD name type_
       : ValD (VarP name) (NormalB expression) []
       : InfixD fixity name
       : []

declareInfixFmapForFunctorCompositionOfDegree :: Int -> Q [Dec]
declareInfixFmapForFunctorCompositionOfDegree = declareInfixWithDegree fmapExpressionOfDegree fmapTypeOfDegree (Fixity 4 InfixL) '$' Both

declareInfixFmapConstForFunctorCompositionOfDegree :: Int -> Q [Dec]
declareInfixFmapConstForFunctorCompositionOfDegree = declareInfixWithDegree fmapConstExpressionOfDegree fmapConstTypeOfDegree (Fixity 4 InfixL) '$' L

pattern x :> y = AppT ArrowT x `AppT` y

(<$$>) :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap fmap fmap
infixl 1 <$$>

declareFlippedInfixFmapForFunctorCompositionOfDegree :: Int -> Q [Dec]
declareFlippedInfixFmapForFunctorCompositionOfDegree = do
  let flipExpression = AppE $ VarE (mkName "flip")
      flipType (ForallT variables constraints (a :> (b :> c))) = ForallT variables constraints (b ~> (a ~> c))
      flipType _ = error "The impossible happened!"
  declareInfixWithDegree (flipExpression <$$> fmapExpressionOfDegree) (flipType <$$> fmapTypeOfDegree) (Fixity 1 InfixL) '&' Both

declareFlippedInfixFmapConstForFunctorCompositionOfDegree :: Int -> Q [Dec]
declareFlippedInfixFmapConstForFunctorCompositionOfDegree = do
  let flipExpression = AppE $ VarE (mkName "flip")
      flipType (ForallT variables constraints (a :> (b :> c))) = ForallT variables constraints (b ~> (a ~> c))
      flipType _ = error "The impossible happened!"
  declareInfixWithDegree (flipExpression <$$> fmapConstExpressionOfDegree) (flipType <$$> fmapConstTypeOfDegree) (Fixity 1 InfixL) '$' R

{-# DEPRECATED declareInfixFmapN "Use 'declareInfixFmapForFunctorCompositionOfDegree' and/or reconsider your life choices." #-}
declareInfixFmapN :: Int -> Q [Dec]
declareInfixFmapN = declareInfixFmapForFunctorCompositionOfDegree

{-# DEPRECATED declareInfixPamfN "Use 'declareFlippedInfixFmapForFunctorCompositionOfDegree' and/or reconsider your life choices." #-}
declareInfixPamfN :: Int -> Q [Dec]
declareInfixPamfN = declareFlippedInfixFmapForFunctorCompositionOfDegree
