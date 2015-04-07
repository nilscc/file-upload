{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Files where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Acid.Advanced
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base32 as Base32
import System.Directory

-- local imports
import Acid.Files
import Types

localFiles :: MonadIO m => m (AcidState Files)
localFiles = do
  liftIO $ openLocalState $ Files Map.empty

newFile
  :: UpMonad m
  => FilePath         -- ^ Old (temporary) file location
  -> FileDescription
  -> m (FileID, FileDescription)
newFile loc fd = do

  -- generate file ID
  gen <- get
  let (fid, gen') = randomFileID gen
  put gen'

  -- set new file location (based on file ID)
  let fd' = fd { fileLocation = "files/" ++ takeWhile ('=' /=) (B8.unpack (Base32.encode fid)) }

  -- try to add file
  st <- asks fileState
  success <- update' st $ AddFileDescription fid fd'

  if success then do
    -- move temporary file
    liftIO $ createDirectoryIfMissing False "files"
    liftIO $ renameFile loc (fileLocation fd')
    -- return file ID and new file description
    return (fid, fd')
   else
    newFile loc fd'

lookupFileDescription :: UpMonad m => FileID -> m (Maybe FileDescription)
lookupFileDescription fid = do
  st <- asks fileState
  query' st $ LookupFileDescription fid

mostRecentFileDescriptions
  :: UpMonad m
  => Int  -- ^ Offset
  -> Int  -- ^ Limit
  -> m [(FileID, FileDescription)]
mostRecentFileDescriptions off lim = do
  st <- asks fileState
  query' st $ MostRecentFileDescriptions off lim

removeFileDescription
  :: UpMonad m
  => FileID
  -> m ()
removeFileDescription fid = do
  st <- asks fileState
  update' st $ RemoveFileDescription fid
