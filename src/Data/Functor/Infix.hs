{-# LANGUAGE TemplateHaskell #-}

module Data.Functor.Infix where

import Data.Functor.Infix.TH (declareInfixFmapN, declareInfixPamfN)
import Data.Functor.Infix.Utilities (concatMapM)

$(concatMapM declareInfixFmapN [1..100])
$(concatMapM declareInfixPamfN [1..100])
