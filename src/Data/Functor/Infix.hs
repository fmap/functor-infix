{-# LANGUAGE TemplateHaskell #-}

module Data.Functor.Infix where

import Data.Functor.Infix.TH (declareInfixFmapN)
import Data.Monoid (Monoid(mconcat))

$(fmap mconcat $ mapM declareInfixFmapN [1..100]) -- No one will ever need 'fmap' composed more than a hundred times.
