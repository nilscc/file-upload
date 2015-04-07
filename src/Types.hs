{-# LANGUAGE ConstraintKinds #-}

module Types where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Crypto.Random
import Data.Acid
import Happstack.Server

import qualified Acid.Files    as Acid
import qualified Acid.Sessions as Acid

type UpMonad m =
  ( MonadState SystemRNG m
  , MonadReader AcidStates m
  , ServerMonad m
  , WebMonad Response m
  , FilterMonad Response m
  , HasRqData m
  , MonadIO m
  , MonadPlus m
  , Functor m
  )

data AcidStates = AcidStates
  { fileState    :: AcidState Acid.Files
  , sessionState :: AcidState Acid.Sessions
  }
