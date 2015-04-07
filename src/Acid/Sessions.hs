{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Acid.Sessions where

import Control.Monad.Reader
import Control.Monad.State
import Crypto.Random

import qualified Data.ByteString as Strict

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Data.Time

type SessionID  = Strict.ByteString
type SessionKey = Strict.ByteString

-- | A single session entry
data SessionData = SessionData
  { sessionCreatedAt :: UTCTime
  }
  deriving (Show, Typeable)

deriveSafeCopy 0 'base ''SessionData

-- | The main session data type
data Sessions = Sessions
  { sessionKey :: SessionKey
  , sessions   :: Map SessionID SessionData
  }
  deriving (Show, Typeable)

deriveSafeCopy 0 'base ''Sessions

randomKey :: CPRG gen => gen -> (SessionKey, gen)
randomKey gen = cprgGenerate 32 gen

randomId :: CPRG gen => gen -> (SessionID, gen)
randomId = randomKey

-- | Set new session key - resets all existing sessions!
setSessionKey :: SessionKey -> Update Sessions ()
setSessionKey skey = do
  modify $ \s -> s { sessionKey = skey, sessions = Map.empty }

getSessionKey :: Query Sessions SessionKey
getSessionKey = asks sessionKey

-- | Test if a session is valid
validSession :: SessionID -> Query Sessions Bool
validSession sid = asks $ (sid `Map.member`) . sessions

-- | Add new session
addSession
  :: SessionKey
  -> SessionID
  -> UTCTime      -- ^ Current time
  -> Update Sessions Bool
addSession key sid now = do
  skey <- liftQuery $ asks sessionKey

  if Strict.null skey || skey /= key then

    -- keys invalid, no success/quit
    return False

   else do

    -- create new session
    let sdata = SessionData { sessionCreatedAt = now }

    -- modify acid state
    modify $ \s -> s { sessions = Map.insert sid sdata (sessions s) }

    -- success
    return True

-- | Remove session
deleteSession :: SessionID -> Update Sessions ()
deleteSession sid = modify $ \s -> s { sessions = Map.delete sid (sessions s) }

-- | Clean up old sessions
cleanupSessions
  :: UTCTime    -- ^ Current time
  -> Update Sessions ()
cleanupSessions now = do
  modify $ \s -> s { sessions = Map.filter sdataFilter (sessions s) }
 where

  sdataFilter sdata =
    -- remove sessions older than 30 days
    diffUTCTime now (sessionCreatedAt sdata) < month

  month = 60 * 60 * 24 * 30 -- in seconds

makeAcidic ''Sessions
  [ 'setSessionKey
  , 'getSessionKey
  , 'validSession
  , 'addSession
  , 'deleteSession
  , 'cleanupSessions ]

