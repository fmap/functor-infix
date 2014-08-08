{-# LANGUAGE TemplateHaskell #-}

module Data.Functor.Infix.Definitions where

import Data.Functor.Infix.TH (declareInfixFmapN, declareInfixPamfN)
import Data.Functor.Infix.Utilities (concatMapM)

$(concatMapM declareInfixFmapN [1..20])
$(concatMapM declareInfixPamfN [1..20])
