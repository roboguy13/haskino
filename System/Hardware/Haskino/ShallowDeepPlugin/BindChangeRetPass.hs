-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.BindChangeRetPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Local bind return type change pass
-- f :: a -> ... -> c -> d ==> f :: a  -> ... -> c -> Expr d
-- It does this by inserting a rep_ <$> to the last expresion of the bind
-- chain for the local bind.
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.BindChangeRetPass (bindChangeRetPass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Control.Monad.Reader

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

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

bindChangeRetPass :: ModGuts -> CoreM ModGuts
bindChangeRetPass guts =
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM changeRetBind) x) (BindEnv guts))) guts

-- | Skip any contexts to get to the actual type part
splitTyConSkippingClasses_maybe :: Type -> Maybe (TyCon, [Type])
splitTyConSkippingClasses_maybe x =
  case splitTyConApp_maybe x of
    split@(Just (left, [right]))
      | isClassTyCon left -> splitTyConSkippingClasses_maybe right
      | otherwise         -> split
    _                     -> Nothing

changeRetBind :: CoreBind -> BindM CoreBind
changeRetBind bndr@(NonRec b e) = do
    df <- liftCoreM getDynFlags
    let (argTys, retTy) = splitFunTys $ varType b
    let tyCon_m = splitTyConSkippingClasses_maybe retTy
    monadTyCon <- thNameToTyCon monadTyConTH
    unitTyCon <- thNameToTyCon ''()
    let unitTyConTy = mkTyConTy unitTyCon
    case tyCon_m of
        -- We are looking for return types of Arduino a
        Just (retTyCon, [retTy']) | pprTraceIt "retTyCon: " retTyCon == monadTyCon &&
                                      not (retTy' `eqType` unitTyConTy) -> do
            let tyCon_m' = splitTyConApp_maybe retTy'
            case tyCon_m' of
                -- We do not want types of Arduino (Expr a), so we look for an
                -- empty list of types, and just a TyCon from the tyCon_m'.
                Just (retTyCon', []) -> do
                    let (bs, e') = collectBinders e

                    exprTyCon <- thNameToTyCon exprTyConTH
                    let exprTyConApp = mkTyConApp exprTyCon [retTy']

                    -- Change the return
                    e'' <- fmapRepBindReturn e'

                    -- Change binding type
                    let b' = setVarType b $ mkFunTys argTys (mkTyConApp retTyCon [exprTyConApp])
                    return (NonRec b' (mkCoreLams bs e''))
                _ -> return bndr
        _ -> return bndr
changeRetBind (Rec bs) = do
    return $ Rec bs
