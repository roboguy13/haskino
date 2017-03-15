-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.RepPushPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.XlatEntry where

import CoreMonad
import GhcPlugins
import Control.Monad.Reader

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

import Data.Boolean
import System.Hardware.Haskino

data BindEnv
    = BindEnv
      { pluginModGuts :: ModGuts
      }

newtype BindM a = BindM { runBindM :: ReaderT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadReader BindEnv)

instance PassCoreM BindM where
  liftCoreM = BindM . ReaderT . const
  getModGuts = BindM $ ReaderT (return . pluginModGuts)

data XlatEntry = XlatEntry {  fromId         :: BindM Id
                            , toId           :: BindM Id
                           }

funcInGivenXlatList :: [XlatEntry] -> Id -> BindM (Maybe XlatEntry)
funcInGivenXlatList xlatList id = do
  funcInXlatList' id xlatList
    where
      funcInXlatList' :: Id -> [XlatEntry] -> BindM (Maybe XlatEntry)
      funcInXlatList' id [] = return Nothing
      funcInXlatList' id (xl:xls) = do
          fId <- fromId xl
          if fId == id
          then return $ Just xl
          else funcInXlatList' id xls

