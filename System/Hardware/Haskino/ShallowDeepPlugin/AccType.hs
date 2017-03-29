module System.Hardware.Haskino.ShallowDeepPlugin.AccType where

-- Accelerate type for shallow embedding
-- (in the form of a "FunList": http://www.twanvl.nl/blog/haskell/non-regular1)

import Data.Array.Accelerate

data AccS a b
  = AccDone (Acc (Plain b))
  | AccStep (Acc (Plain a)) (AccS (Acc (Plain a)) (a -> b))

