-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.RepPushPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.HaskinoXlatList where

import CoreMonad
import GhcPlugins
import Language.Haskell.TH

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

import Data.Boolean
import System.Hardware.Haskino

import System.Hardware.Haskino.ShallowDeepPlugin.XlatEntry

-- The following table defines the names of the Shallow DSL functions
-- to translate from and the Deep DSL functions to translate to.
xlatList :: [XlatEntry]
xlatList = [  XlatEntry (thNameToId 'not)
                        (thNameToId 'Data.Boolean.notB)
            , XlatEntry (thNameToId '(||))
                        (thNameToId '(||*))
            , XlatEntry (thNameToId '(&&))
                        (thNameToId '(&&*))
            , XlatEntry (thNameToId '(==))
                        (thNameToId 'eqE)
            , XlatEntry (thNameToId '(/=))
                        (thNameToId 'neqE)
            , XlatEntry (thNameToId '(>))
                        (thNameToId 'greatE)
            , XlatEntry (thNameToId '(<))
                        (thNameToId 'lessE)
            , XlatEntry (thNameToId '(>=))
                        (thNameToId 'greateqE)
            , XlatEntry (thNameToId '(<))
                        (thNameToId 'lesseqE)
            , XlatEntry (thNameToId '(+))
                        (thNameToId '(+))
           ]

