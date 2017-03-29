-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.HaskinoXlatList
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
-------------------------------------------------------------------------------

module System.Hardware.Haskino.ShallowDeepPlugin.AccXlatList where

import CoreMonad
import GhcPlugins
import Language.Haskell.TH

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

import System.Hardware.Haskino.ShallowDeepPlugin.XlatEntry

import Data.Boolean
import Data.Array.Accelerate

xlatList :: [XlatEntry]
xlatList = [  XlatEntry (stringToId "not")
                        (stringToId "Data.Accelerate.not")
            , XlatEntry (stringToId "(||)")
                        (stringToId "(||*)")
            , XlatEntry (stringToId "(&&)")
                        (stringToId "(&&*)")
            , XlatEntry (stringToId "(==)")
                        (stringToId "(==*)")
            , XlatEntry (stringToId "(/=)")
                        (stringToId "(/=*)")
            , XlatEntry (stringToId "(>)")
                        (stringToId "(>*)")
            , XlatEntry (stringToId "(<)")
                        (stringToId "(<*)")
            , XlatEntry (stringToId "(>=)")
                        (stringToId "(>=*)")
            , XlatEntry (stringToId "(<)")
                        (stringToId "(<*)")
            , XlatEntry (stringToId "(+)")
                        (stringToId "(+*)")
           ]

