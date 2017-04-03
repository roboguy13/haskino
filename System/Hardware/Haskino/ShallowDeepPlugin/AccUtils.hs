-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.Utils
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
-------------------------------------------------------------------------------
-- NOTE: Not in a working state yet
{-# LANGUAGE ConstraintKinds, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.AccUtils where

import Data.Functor

import qualified Data.Array.Accelerate
import           Data.Array.Accelerate (Exp, lift, unlift, Lift, Unlift, Plain, Elt)

import           System.Hardware.Haskino.ShallowDeepPlugin.AccType

-- class (Lift Exp a, a ~ Plain a) => AExp a
-- instance (Lift Exp a, a ~ Plain a) => AExp a

type AExp a = (Lift Exp a, a ~ Plain a, Elt a)

abs_ :: Exp a -> a
abs_ _ = error "abs_ called"

rep_ :: AExp a => a -> Exp a
rep_ = lift

exprClassTyConTH     = ''System.Hardware.Haskino.ShallowDeepPlugin.AccUtils.AExp
exprTyConTH          = ''Data.Array.Accelerate.Exp
monadCondTyConTH     = ''Data.Array.Accelerate.Elt
monadTyConTH         = ''System.Hardware.Haskino.ShallowDeepPlugin.AccType.ExpM
absNameTH            = 'System.Hardware.Haskino.ShallowDeepPlugin.AccUtils.abs_
repNameTH            = 'System.Hardware.Haskino.ShallowDeepPlugin.AccUtils.rep_
ifThenElseNameTH     = 'Data.Array.Accelerate.cond
ifThenElseUnitNameTH = 'Data.Array.Accelerate.cond -- DCY: Is this what it needs to be?

-- Taken from HaskinoUtils.hs:
functTyConTH         = ''Data.Functor.Functor
unitTyConTH          = ''()
bindNameTH           = '(>>=)
bindThenNameTH       = '(>>)
falseNameTH          = 'Prelude.False
fmapNameTH           = '(<$>)
returnNameTH         = 'Prelude.return


