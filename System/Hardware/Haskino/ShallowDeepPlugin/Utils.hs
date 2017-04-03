-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.Utils
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Shallow Deep Plugin Utility Functions
-------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module System.Hardware.Haskino.ShallowDeepPlugin.Utils (absExpr,
                                           buildDictionaryT,
                                           buildDictionaryTyConT,
                                           fmapAbsExpr,
                                           fmapRepBindReturn,
                                           fmapRepExpr,
                                           repExpr,
                                           stringToId,
                                           stringToId_maybe,
                                           thNameToId,
                                           thNameToTyCon,
                                           thNameTyToDict,
                                           thNameTyToTyConApp,
                                           PassCoreM(..),
                                           pattern (:$),
                                           -- DSL specific names
                                           exprClassTyConTH,
                                           exprTyConTH,
                                           monadCondTyConTH,
                                           monadTyConTH,
                                           absNameTH,
                                           repNameTH,
                                           ifThenElseNameTH,
                                           ifThenElseUnitNameTH,
                                           -- General Haskell names
                                           bindNameTH,
                                           functTyConTH,
                                           unitTyConTH,
                                           bindThenNameTH,
                                           falseNameTH,
                                           fmapNameTH,
                                           returnNameTH) where

import CoreMonad
import GhcPlugins
import HscTypes
import Data.Char
import TcRnMonad
import TcSMonad
import TcSimplify
import TcEvidence
import ErrUtils
import DsBinds
import DsMonad (initDsTc)
import Control.Arrow (first, second)
import Control.Monad
import Data.Functor
import Encoding (zEncodeString)
import qualified Language.Haskell.TH as TH

import System.Hardware.Haskino.ShallowDeepPlugin.Typechecker (initTcFromModGuts)

import System.Hardware.Haskino.ShallowDeepPlugin.AccUtils



-- An infix pattern synonym for `App` to make applications with multiple
-- arguments easier to manipulate:
infixl 0 :$
pattern f :$ x = App f x

class (Monad m, MonadIO m) => PassCoreM m where
    -- | 'CoreM' can be lifted to this monad.
    liftCoreM  :: CoreM a -> m a
    getModGuts :: m ModGuts

instance PassCoreM CoreM where
  liftCoreM = id
  getModGuts = error "Cannot get modguts from CoreM"

thNameToId :: PassCoreM m => TH.Name -> m Id
thNameToId n = do
  name_m <- liftCoreM $ thNameToGhcName n
  case name_m of
    (Just name) -> liftCoreM $ lookupId name
    _           -> error "Unable to Lookup ID"

stringToId_maybe :: PassCoreM m => String -> m (Maybe Id)
stringToId_maybe str = do
  let lookId x = do
        id <- liftCoreM $ lookupId $ gre_name x
        return $ Just id
  guts <- getModGuts
  let gres = lookupGlobalRdrEnv (mg_rdr_env guts) (mkVarOcc str)
  case gres of
    []   -> return Nothing
    [x]  -> lookId x
    x:xs -> lookId x -- Need to fix this, if there are multiples, need to
                     -- find one we are looking for.

stringToId :: PassCoreM m => String -> m Id
stringToId str = do
  id_m <- stringToId_maybe str
  case id_m of
    (Just id) -> return id
    _         -> error $ "Error unable to Lookup ID " ++ str ++ "."

thNameToTyCon :: PassCoreM m => TH.Name -> m TyCon
thNameToTyCon n = do
  name_m <- liftCoreM $ thNameToGhcName n
  case name_m of
    (Just name) -> liftCoreM $ lookupTyCon name
    _           -> error "Unable to Lookup TyCon"

thNameTyToDict :: PassCoreM m => TH.Name -> Type -> m CoreExpr
thNameTyToDict n ty = do
  tyCon <- thNameToTyCon n
  buildDictionaryTyConT tyCon ty

thNameTyToTyConApp :: PassCoreM m => TH.Name -> Type -> m Type
thNameTyToTyConApp n ty = do
  tyCon <- thNameToTyCon n
  return $  mkTyConApp tyCon [ty]

repExpr :: PassCoreM m => CoreExpr -> m CoreExpr
repExpr e = do
    let ty = exprType e
    repId <- thNameToId repNameTH
    repDict <- thNameTyToDict exprClassTyConTH ty
    return $ mkCoreApps (Var repId) [Type ty, repDict, e]

absExpr :: PassCoreM m => CoreExpr -> m CoreExpr
absExpr e = do
    let ty = exprType e
    absId <- thNameToId absNameTH
    return $ mkCoreApps (Var absId) [Type ty, e]

fmapAbsExpr :: PassCoreM m => Type -> Type -> CoreExpr -> m CoreExpr
fmapAbsExpr tyConTy ty e = do
    absId <- thNameToId absNameTH

    exprTyConApp <- thNameTyToTyConApp exprTyConTH ty

    fmapId <- thNameToId fmapNameTH
    functDict <- thNameTyToDict functTyConTH tyConTy

    let absApp = mkCoreApps (Var absId) [Type ty]
    return $ mkCoreApps (Var fmapId) [Type tyConTy, Type exprTyConApp, Type ty,
                                      functDict, absApp, e]

fmapRepExpr :: PassCoreM m => Type -> Type -> CoreExpr -> m CoreExpr
fmapRepExpr tyConTy ty e = do
    repId <- thNameToId repNameTH
    repDict <- thNameTyToDict exprClassTyConTH ty

    exprTyConApp <- thNameTyToTyConApp exprTyConTH ty

    fmapId <- thNameToId fmapNameTH
    functDict <- thNameTyToDict functTyConTH tyConTy

    let repApp = mkCoreApps (Var repId) [Type ty, repDict]
    return $ mkCoreApps (Var fmapId) [Type tyConTy, Type ty, Type exprTyConApp,
                                      functDict, repApp, e]

fmapRepBindReturn :: PassCoreM m => CoreExpr -> m CoreExpr
fmapRepBindReturn e = do
    let (bs, e') = collectBinders e
    let (f, args) = collectArgs e'
    bindId <- thNameToId bindNameTH
    thenId <- thNameToId bindThenNameTH
    case f of
      Var fv -> do
        if fv == bindId || fv == thenId
        then do
            la' <- fmapRepBindReturn $ last args
            let args' = init args ++ [la']
            return $ mkLams bs (mkCoreApps f args')
        else do
            let (tyCon,[ty']) = splitTyConApp $ exprType e'
            retExpr <- fmapRepExpr (mkTyConTy tyCon) ty' e'
            return $ mkLams bs retExpr
      _ -> return e

-- Adapted from HERMIT.Monad
runTcM :: PassCoreM m => TcM a -> m a
runTcM m = do
    env <- liftCoreM getHscEnv
    dflags <- liftCoreM getDynFlags
    guts <- getModGuts
    (msgs, mr) <- liftIO $ initTcFromModGuts env guts HsSrcFile False m
    let showMsgs (warns, errs) = showSDoc dflags $ vcat
                                                 $    text "Errors:" : pprErrMsgBagWithLoc errs
                                                   ++ text "Warnings:" : pprErrMsgBagWithLoc warns
    maybe (fail $ showMsgs msgs) return mr

newCondName :: PassCoreM m => String -> m Name
newCondName nm = mkSystemVarName <$> (liftCoreM getUniqueM) <*> return (mkFastString nm)

newIdH :: PassCoreM m => String -> Type -> m Id
newIdH name ty = do name' <- newCondName name
                    return $ mkLocalId name' ty

-- Adapted from HERMIT
buildDictionary :: PassCoreM m => Id -> m (Id, [CoreBind])
buildDictionary evar = do
    runTcM $ do
#if __GLASGOW_HASKELL__ > 710
        loc <- getCtLocM (GivenOrigin UnkSkol) Nothing
#else
        loc <- getCtLoc $ GivenOrigin UnkSkol
#endif
        let predTy = varType evar
#if __GLASGOW_HASKELL__ > 710
            nonC = mkNonCanonical $ CtWanted { ctev_pred = predTy, ctev_dest = EvVarDest evar, ctev_loc = loc }
            wCs = mkSimpleWC [cc_ev nonC]
        (_wCs', bnds) <- second evBindMapBinds <$> runTcS (solveWanteds wCs)
#else
            nonC = mkNonCanonical $ CtWanted { ctev_pred = predTy, ctev_evar = evar, ctev_loc = loc }
            wCs = mkSimpleWC [nonC]
        (_wCs', bnds) <- solveWantedsTcM wCs
#endif
        bnds1 <- initDsTc $ dsEvBinds bnds
        return (evar, bnds1)

buildDictionaryT :: PassCoreM m => Type -> m CoreExpr
buildDictionaryT ty = do
    dflags <- liftCoreM getDynFlags
    binder <- newIdH ("$d" ++ zEncodeString (filter (not . isSpace) (showPpr dflags ty))) ty
    (i,bnds) <- buildDictionary binder
    return $ case bnds of
                [NonRec v e] | i == v -> e -- the common case that we would have gotten a single non-recursive let
                _ -> mkCoreLets bnds (varToCoreExpr i)

buildDictionaryTyConT :: PassCoreM m => TyCon -> Type -> m CoreExpr
buildDictionaryTyConT tyCon ty =
    buildDictionaryT $ GhcPlugins.mkTyConApp tyCon [ty]
