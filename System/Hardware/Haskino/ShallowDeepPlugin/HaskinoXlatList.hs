-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.HaskinoXlatList
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

import System.Hardware.Haskino.ShallowDeepPlugin.XlatEntry

import Data.Boolean
import System.Hardware.Haskino
import qualified System.Hardware.Haskino

-- The following table defines the names of the Shallow DSL functions
-- to translate from and the Deep DSL functions to translate to.
xlatList :: [XlatEntry]
xlatList = [  XlatEntry (stringToId "not")
                        (stringToId "Data.Boolean.notB")
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

            -- For use in CommProcPass.hs
commProcPassXlatList :: [XlatEntry]
commProcPassXlatList = [  XlatEntry (thNameToId 'System.Hardware.Haskino.loop)
                                    (thNameToId 'System.Hardware.Haskino.loopE)
                        , XlatEntry (thNameToId 'System.Hardware.Haskino.setPinMode)
                                    (thNameToId 'System.Hardware.Haskino.setPinModeE)
                        , XlatEntry (thNameToId 'System.Hardware.Haskino.digitalRead)
                                    (thNameToId 'System.Hardware.Haskino.digitalReadE)
                        , XlatEntry (thNameToId 'System.Hardware.Haskino.digitalWrite)
                                    (thNameToId 'System.Hardware.Haskino.digitalWriteE)
                        , XlatEntry (thNameToId 'System.Hardware.Haskino.delayMillis)
                                    (thNameToId 'System.Hardware.Haskino.delayMillisE)
                       ]

