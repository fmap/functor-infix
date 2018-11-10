{-# LANGUAGE TemplateHaskell #-}

module Data.Functor.Infix.Definitions
  ( module Data.Functor.Infix.Definitions
  , (<$>)
  ) where

import Control.Monad (msum)
import Data.Functor ((<$>))
import Data.Functor.Infix.TH
 ( declareInfixFmapForFunctorCompositionOfDegree
 , declareFlippedInfixFmapForFunctorCompositionOfDegree
 , declareInfixFmapConstForFunctorCompositionOfDegree
 , declareFlippedInfixFmapConstForFunctorCompositionOfDegree
 )

$(fmap msum $ mapM declareInfixFmapForFunctorCompositionOfDegree [2..20])
-- from 1 because `<&>` not found elsewhere
$(fmap msum $ mapM declareFlippedInfixFmapForFunctorCompositionOfDegree [1..20])
$(fmap msum $ mapM declareInfixFmapConstForFunctorCompositionOfDegree [2..20])
$(fmap msum $ mapM declareFlippedInfixFmapConstForFunctorCompositionOfDegree [2..20])
