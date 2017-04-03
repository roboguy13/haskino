{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module System.Hardware.Haskino.ShallowDeepPlugin.AccType where

import Data.Array.Accelerate
import Control.Monad.Identity

newtype ExpM a = ExpM { getExpM :: a }
  deriving (Functor, Prelude.Eq, Prelude.Ord, Prelude.Num)

instance Applicative ExpM where
  pure  = return
  (<*>) = ap

instance Monad ExpM where
  return = ExpM
  ExpM x >>= f = f x

expM :: a -> ExpM a
expM = ExpM

runExpM :: (Plain a ~ a, Lift Exp a) => ExpM a -> Exp a
runExpM = runE . getExpM

-- Another idea (use a run function and combine the "transform everything
-- of a certain type approach" and the "use a run function and push rep in"
-- approach):

runE :: (Plain a ~ a, Lift Exp a) => a -> Exp a
runE = lift

