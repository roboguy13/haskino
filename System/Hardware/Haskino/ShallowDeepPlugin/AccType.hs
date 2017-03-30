module System.Hardware.Haskino.ShallowDeepPlugin.AccType where

-- Accelerate type for shallow embedding
-- (in the form of a "FunList": http://www.twanvl.nl/blog/haskell/non-regular1)

import Data.Array.Accelerate

data AccS a b
  = AccDone (Acc (Plain b))
  | AccStep (Acc (Plain a)) (AccS (Acc (Plain a)) (a -> b))





-- Another idea (use a run function and combine the "transform everything
-- of a certain type approach" and the "use a run function and push rep in"
-- approach)

run :: a -> Acc a
run _ = undefined -- TODO: Implement

