-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.Utils
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
-------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.HaskinoUtils where

import Data.Functor

-- The following line contain the imports specific to the DSL language
-- being trnasformed, as well as Template Haskell definintions of the
-- DSL Monad and Expr types, names for the Worker/Wrapper abs/rep,
-- and names of the conditionals in the DSL.
import qualified System.Hardware.Haskino

exprClassTyConTH     = ''System.Hardware.Haskino.ExprB
exprTyConTH          = ''System.Hardware.Haskino.Expr
monadCondTyConTH     = ''System.Hardware.Haskino.ArduinoConditional
monadTyConTH         = ''System.Hardware.Haskino.Arduino
absNameTH            = 'System.Hardware.Haskino.abs_
repNameTH            = 'System.Hardware.Haskino.rep_
ifThenElseNameTH     = 'System.Hardware.Haskino.ifThenElseE
ifThenElseUnitNameTH = 'System.Hardware.Haskino.ifThenElseUnitE

functTyConTH         = ''Data.Functor.Functor
unitTyConTH          = ''()
bindNameTH           = '(>>=)
bindThenNameTH       = '(>>)
falseNameTH          = 'Prelude.False
fmapNameTH           = '(<$>)
returnNameTH         = 'Prelude.return

