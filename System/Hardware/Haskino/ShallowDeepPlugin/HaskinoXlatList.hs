-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.RepPushPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-------------------------------------------------------------------------------

module System.Hardware.Haskino.ShallowDeepPlugin.HaskinoXlatList where

import CoreMonad
import GhcPlugins
import Language.Haskell.TH

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

import System.Hardware.Haskino.ShallowDeepPlugin.XlatEntry

import Data.Boolean
import System.Hardware.Haskino

-- The following table defines the names of the Shallow DSL functions
-- to translate from and the Deep DSL functions to translate to.
xlatList :: [XlatEntry]
xlatList = [  XlatEntry (stringToId "not")
                        (stringToId "Data.Boolean.notB")
            , XlatEntry (stringToId "(||)")
                        (stringToId "(||*)")
            , XlatEntry (stringToId "(&&)")
                        (stringToId "(&&*)")
            , XlatEntry (stringToId "(==)")
                        (stringToId "eqE")
            , XlatEntry (stringToId "(/=)")
                        (stringToId "neqE")
            , XlatEntry (stringToId "(>)")
                        (stringToId "greatE")
            , XlatEntry (stringToId "(<)")
                        (stringToId "lessE")
            , XlatEntry (stringToId "(>=)")
                        (stringToId "greateqE")
            , XlatEntry (stringToId "(<)")
                        (stringToId "lesseqE")
            , XlatEntry (stringToId "(+)")
                        (stringToId "(+)")
           ]

