{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Page.Index where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base32 as Base32
import Control.Monad
import Control.Monad.Trans
import Happstack.Server

import Files
import Sessions
import Types
import Acid.Sessions (SessionID) -- , SessionKey)

import qualified Page.Html.Index as Html
import qualified Page.Html.Login as Html

index :: UpMonad m => m Response
index = msum
  [ requireSession startPage
  , loginPage
  ]

startPage :: UpMonad m => SessionID -> m Response
startPage _sid = do
  files <- mostRecentFileDescriptions 0 50
  ok $ toResponse $ Html.index files

--
-- Login
--

loginPage :: UpMonad m => m Response
loginPage = msum
  [ method [POST] >> performLogin
  , ok $ toResponse Html.login
  ]

performLogin :: UpMonad m => m Response
performLogin = do

  -- lookup user input and decode as base32 bytestring:
  inp <- lookBS "session-key"
  case Base32.decode $ BL.toStrict inp of

    -- parse/decode error
    Left err -> do
      liftIO $ putStrLn $ "Invalid login: " ++ show inp ++ " (" ++ err ++ ")"
      mzero

    Right skey -> do
      -- try to create new session
      res <- newSession skey
      case res of
        Just sid -> startPage sid
        Nothing  -> do
          liftIO $ putStrLn $ "Failed login: " ++ show inp
          mzero
