{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Files where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Acid.Advanced
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base32 as Base32
import System.Directory

-- local imports
import Acid.Files
import Types

localFiles :: MonadIO m => m (AcidState Files)
localFiles = do
  liftIO $ openLocalState $ Files Map.empty Map.empty

-- | Create a new file entry
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
  let cs  = map toLower $ takeWhile ('=' /=) $ B8.unpack $ Base32.encode fid
      fd' = fd { fileLocation = "files/" ++ cs }

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

removeFileDescription
  :: UpMonad m
  => FileID
  -> m ()
removeFileDescription fid = do
  st <- asks fileState
  update' st $ RemoveFileDescription fid

-- | Create an entry for a partially uploaded file
newPartialFile
  :: UpMonad m
  => FilePath         -- ^ Temporary file location
  -> FileDescription
  -> PartialFileStatus
  -> m FileDescription
newPartialFile loc fd pfs = do

  -- set partial file location (based on file ID)
  let cs  = takeWhile ('=' /=) $ partialFileChecksum pfs
      fd' = fd { fileLocation = "files/" ++ cs ++ ".prt" }

  -- add partial file to state
  st <- asks fileState
  update' st $ AddPartialFile fd' pfs

  liftIO $ createDirectoryIfMissing False "files"
  liftIO $ renameFile loc (fileLocation fd')

  return fd'

-- | Get all partial files
partialFiles :: UpMonad m => m (Map PartialFileStatus FileDescription)
partialFiles = do
  st <- asks fileState
  query' st $ GetPartialFiles

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

findPartialFileDescription
  :: UpMonad m
  => PartialFileStatus
  -> m (Maybe FileDescription)
findPartialFileDescription pfs = do
  st <- asks fileState
  query' st $ FindPartialFileDescription pfs

removePartialFileDescription
  :: UpMonad m
  => PartialFileStatus
  -> m ()
removePartialFileDescription pfs = do
  st <- asks fileState
  update' st $ RemovePartialFileDescription pfs
