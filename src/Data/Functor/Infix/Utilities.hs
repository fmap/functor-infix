module Data.Functor.Infix.Utilities (concatMapM) where

import Data.Monoid (Monoid(mconcat))

concatMapM :: Functor m => Monad m => Monoid b => (a -> m b) -> [a] -> m b
concatMapM = fmap (fmap mconcat) `fmap` mapM -- It'd be really convenient to have <$$$> here. ;-)
