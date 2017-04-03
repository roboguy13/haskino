{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
-- {-# LANGUAGE RebindableSyntax #-}

module Main where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as A
import           Data.Array.Accelerate (Acc, Exp, (:.), Scalar)
-- import           Data.Array.Accelerate
import System.Hardware.Haskino.ShallowDeepPlugin.AccType

eventuallyTrue :: ExpM Int -> ExpM Bool
eventuallyTrue n =
  if n == 0
    then return True
    else eventuallyTrue (n-1)
-- eventuallyTrue 0 = A.lift True
-- eventuallyTrue n = eventuallyTrue (n-1)

accTest :: ExpM Int
accTest = do
  b <- eventuallyTrue 5
  if b then 1 else 0

-- test :: Bool
-- test = (show accTest) == (show accTestE)

main :: IO ()
main = print $ (`A.indexArray` A.Z) $ A.run $ A.unit $ runExpM accTest

