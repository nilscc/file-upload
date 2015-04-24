{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Acid.Files where

import Control.Monad.Reader
import Control.Monad.State
import Crypto.Random
import Data.Acid
import Data.List
import Data.SafeCopy
import Data.Typeable
import Data.Time

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.ByteString as BS

import Data.Text (Text)

type FileID = BS.ByteString

type FileSize = Integer

-- | Checksum as hex string
type Checksum = String

data FileDescription = FileDescription
  { fileName        :: Text
  , fileSize        :: FileSize
  , fileChecksum    :: Checksum
  , fileContentType :: String
  , fileLocation    :: FilePath
  , fileCreatedAt   :: UTCTime
  }
  deriving (Show, Typeable, Eq, Ord)

deriveSafeCopy 0 'base ''FileDescription

data PartialFileStatus = PartialFileStatus
  { partialFileSize     :: FileSize
  , partialFileChecksum :: Checksum
  }
  deriving (Show, Typeable, Eq, Ord)

deriveSafeCopy 0 'base ''PartialFileStatus

data Files = Files
  { files        :: Map FileID FileDescription
  , partialFiles :: Map PartialFileStatus FileDescription
  }
  deriving (Show, Typeable)

deriveSafeCopy 0 'base ''Files

randomFileID :: CPRG gen => gen -> (FileID, gen)
randomFileID gen = cprgGenerate 16 gen

-- | Test if a file ID already exists
uniqueFileID :: FileID -> Query Files Bool
uniqueFileID fid = asks $ not . Map.member fid . files

-- | Add a new file description. Returns `False` and does nothing when ID is
-- not unique
addFileDescription
  :: FileID
  -> FileDescription
  -> Update Files Bool
addFileDescription fid fdesc = do
  unique <- liftQuery $ uniqueFileID fid
  when unique $
    modify $ \f -> f { files = Map.insert fid fdesc (files f) }
  return unique

allFileDescriptions :: Query Files (Map FileID FileDescription)
allFileDescriptions = asks files

-- | Find file with ID
lookupFileDescription :: FileID -> Query Files (Maybe FileDescription)
lookupFileDescription fid = asks $ Map.lookup fid . files

-- | Find files with IDs
lookupFileDescriptions :: [FileID] -> Query Files (Map FileID FileDescription)
lookupFileDescriptions fids = asks $ Map.filterWithKey (\k _ -> k `elem` fids) . files

-- | Get most recently created file descriptions
mostRecentFileDescriptions
  :: Int  -- ^ Offset
  -> Int  -- ^ Limit
  -> Query Files [(FileID, FileDescription)]
mostRecentFileDescriptions off lim =
  asks $ take lim . drop off . sortByDateDesc . Map.toList . files
 where
  sortByDateDesc = sortBy (\(_,a) (_,b) -> fileCreatedAt b `compare` fileCreatedAt a)

removeFileDescription :: FileID -> Update Files ()
removeFileDescription fid = modify $ \f -> f { files = Map.delete fid (files f) }

--------------------------------------------------------------------------------
-- Partial files
--

addPartialFile :: FileDescription -> PartialFileStatus -> Update Files ()
addPartialFile fd pfs =
  modify $ \f -> f { partialFiles = Map.insert pfs fd (partialFiles f) }

getPartialFiles :: Query Files (Map PartialFileStatus FileDescription)
getPartialFiles = asks partialFiles

findPartialFileDescription :: PartialFileStatus -> Query Files (Maybe FileDescription)
findPartialFileDescription pfs = do
  asks $ Map.lookup pfs . partialFiles

removePartialFileDescription :: PartialFileStatus -> Update Files ()
removePartialFileDescription pfs = do
  modify $ \f -> f { partialFiles = Map.delete pfs (partialFiles f) }

makeAcidic ''Files
  [ 'uniqueFileID
  , 'addFileDescription
  , 'allFileDescriptions
  , 'lookupFileDescription
  , 'lookupFileDescriptions
  , 'mostRecentFileDescriptions
  , 'removeFileDescription
  , 'addPartialFile
  , 'getPartialFiles
  , 'findPartialFileDescription
  , 'removePartialFileDescription
  ]
