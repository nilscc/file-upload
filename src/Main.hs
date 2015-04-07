{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Crypto.Random
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception
import System.Directory
import Data.Acid
import Happstack.Server

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base32 as Base32

import Files
import Sessions
import Types
import qualified Page.Files as Page (files)
import qualified Page.Index as Page (index)
import qualified Page.Upload as Page (upload)
import qualified Page.Error as Page (notFound)

main :: IO ()
main = do

  -- get random number generator
  entro <- createEntropyPool
  let gen = (cprgCreate entro) :: SystemRNG

  -- load acid state
  (sst, gen') <- localSessions gen

  -- show session key (base32)
  key <- sessionKey sst
  putStrLn "\n--------------------------------------------------------------------------------"
  putStrLn $ "Session key: " ++ takeWhile ('=' /=) (B8.unpack (Base32.encode key))
  putStrLn "--------------------------------------------------------------------------------\n"

  -- HTTP configuration
  let conf = nullConf { port = 8085 }

  putStrLn $ "Starting server on port " ++ show (port conf)

  -- open file state
  fst <- localFiles

  -- combine states
  let states = AcidStates fst sst

  let webserv = simpleHTTP conf $ runReaderT (evalStateT mainRoute gen') states

  webserv `finally` createCheckpoint sst

mainRoute :: UpMonad m => m Response
mainRoute = do

  tmp_dir <- liftIO getTemporaryDirectory

  -- set max file size policy
  decodeBody $ defaultBodyPolicy tmp_dir
                                 gb -- max file upload size
                                 mb -- max non-file upload
                                 mb -- max multipart size

  -- main route
  msum
    [ nullDir >> Page.index
    , dir "upload" $ method [POST] >> Page.upload
    , dir "files"  $ path Page.files
    , dir "static" $ serveDirectory DisableBrowsing [] "static"
    , Page.notFound
    ]

 where
  gb = 1024 * mb
  mb = 1024 * kb
  kb = 1024 -- bytes
