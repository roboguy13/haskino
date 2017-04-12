{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TypeOperators              #-}

module System.Hardware.Haskino.ShallowDeepPlugin.AccType where

import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter as I
import Control.Monad.Identity

import Control.Natural as N
import Control.Remote.Monad
import Control.Remote.Monad.Packet.Strong as SP

import Data.Constraint

-- data ExpElt a where
--   ExpElt :: Elt a => Exp a -> ExpElt a

-- runExpElt :: Elt a => ExpElt a -> Exp a
-- runExpElt (ExpElt e) = e

type ExpM a = RemoteMonad () Exp a
-- data ExpM a where
--   ExpM :: Elt a => Exp a -> ExpM a

-- data ConstrNT c f g = ConstrNT (forall a. c a => f a -> g a)

-- applyConstrNT :: c a =>
--   ((f :~> g) -> r) ->
--   ConstrNT c f g ->
--   (f a -> g a) ->
--   r
-- applyConstrNT f (ConstrNT nf) fa = _
--   where
--     nt = ConstrNT undefined

runWP :: Elt a => SP.StrongPacket () Exp a -> IO a
runWP (Command x y) = runWP y
runWP (Procedure e) = return $ (`indexArray` Z) $ I.run (unit e)

runExpM :: Elt a => ExpM a -> IO a
runExpM e
  = (N.unwrapNT $ runMonad $ N.wrapNT (\pkt -> runWP pkt)) e


-- newtype ExpM a = ExpM { getExpM :: a }
--   deriving (Functor, Prelude.Eq, Prelude.Ord, Prelude.Num)

-- instance Applicative ExpM where
--   pure  = return
--   (<*>) = ap

-- instance Monad ExpM where
--   return = ExpM
--   ExpM x >>= f = f x

-- expM :: a -> ExpM a
-- expM = ExpM

-- runExpM :: (Plain a ~ a, Lift Exp a) => ExpM a -> Exp a
-- runExpM = runE . getExpM

-- -- Another idea (use a run function and combine the "transform everything
-- -- of a certain type approach" and the "use a run function and push rep in"
-- -- approach):

-- runE :: (Plain a ~ a, Lift Exp a) => a -> Exp a
-- runE = lift

-- data ExpMExp a = ExpMExp (ExpM (Exp a))

-- condM :: Elt a => ExpM (Exp Bool) -> ExpM (Exp a) -> ExpM (Exp a) -> ExpM (Exp a)
-- condM c t f = ExpM $ cond (getExpM c) (getExpM t) (getExpM f)

