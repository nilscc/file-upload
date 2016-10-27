{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Sessions where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base32 as Base32

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Crypto.Random
import Data.Acid
import Data.Acid.Advanced
import Data.Time
import Happstack.Server

import Acid.Sessions (SessionKey, SessionID)
import qualified Acid.Sessions as Acid
import Types

localSessions :: (CPRG gen, MonadIO m) => gen -> m (AcidState Acid.Sessions, gen)
localSessions gen = do

  let (key, gen') = Acid.randomKey gen
      session     = Acid.Sessions key Map.empty
  st <- liftIO $ openLocalState session

  return (st, gen')

sessionKey :: MonadIO m => AcidState Acid.Sessions -> m Acid.SessionKey
sessionKey st = query' st Acid.GetSessionKey

getSessionIdCookie :: (Monad m, HasRqData m, Functor m) => m (Maybe Acid.SessionID)
getSessionIdCookie = do
  val <- lookCookieValue "session-id"
  return $ if null val then Nothing else Just (B8.pack val)

requireSession
  :: UpMonad m
  => (SessionID -> m a)
  -> m a
requireSession go = do
  res <- getSessionIdCookie
  case res of
    Just sid -> do
      -- verify session cookie vs state sessions
      st <- asks sessionState
      guard =<< query' st (Acid.ValidSession sid)
      -- run function
      go sid
    Nothing  -> mzero

setSessionIdCookie :: (MonadIO m, FilterMonad Response m) => Acid.SessionID -> m ()
setSessionIdCookie sid = do

  -- TODO: set 'secure = true'
  let cookie = mkCookie "session-id" (B8.unpack $ Base32.encode sid)
      month  = 60 * 60 * 24 * 30 -- in seconds

  addCookie (MaxAge month) cookie

-- | (Try to) create a new session, given a (user input) session key. Returns
-- `Nothing` if the session key is invalid.
newSession :: UpMonad m => SessionKey -> m (Maybe SessionID)
newSession skey = do

  -- generate session ID
  gen <- get
  let (sid, gen') = Acid.randomId gen
  put gen'

  -- try to add session to acid state
  st <- asks sessionState
  now <- liftIO getCurrentTime
  success <- update' st $ Acid.AddSession skey sid now

  if success then do
    -- set cookie etc
    setSessionIdCookie sid
    return $ Just sid
   else
    return Nothing
