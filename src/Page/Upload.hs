{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Page.Upload where

import Control.Applicative
import Control.Monad.Trans
import Crypto.Hash
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Time
import qualified Data.Text as Text
import Happstack.Server
import System.IO

import Acid.Files (FileDescription(..), PartialFileStatus(..), Checksum)
import Files
import Types
import qualified Page.Error as Page
import Page.Html.FileDescription

-- | Produce the standard string representation of a content-type,
--   e.g. \"text\/html; charset=ISO-8859-1\".
showContentType :: ContentType -> String
showContentType (ContentType x y ps) = x ++ "/" ++ y ++ showParameters ps
 where
  showParameters :: [(String,String)] -> String
  showParameters = concatMap f
   where
    f (n,v) = "; " ++ n ++ "=\"" ++ concatMap esc v ++ "\""
    esc '\\' = "\\\\"
    esc '"'  = "\\\""
    esc c | c `elem` ['\\','"'] = '\\':[c]
          | otherwise = [c]

md5 :: BL.ByteString -> Checksum
md5 bs = B8.unpack $ digestToHexByteString (hashlazy bs :: Digest MD5)

-- Response codes
partialContent, conflict :: Int
partialContent = 206
conflict       = 409

resumeUpload
  :: UpMonad m
  => m Response
resumeUpload = dir "file" $ path $ \partial_checksum -> do

  -- get partial file status
  partial_size <- lookRead "partial-size"
  let pfs = PartialFileStatus partial_size partial_checksum

  -- file info
  (tmp, _, _) <- lookFile "blob"
  fchksm      <- look "checksum"
  -- total size is blob size + partial uploaded size
  bsize       <- lookRead "size"
  let fsize = bsize + partial_size

  -- lookup file description
  mfd <- findPartialFileDescription pfs
  case mfd of
    Just fd | fileSize fd == fsize && fileChecksum fd == fchksm -> do

      -- merge temporary and partial file
      liftIO $ do
        tmp_cont <- BL.hGetContents =<< openFile tmp ReadMode
        withFile (fileLocation fd) AppendMode $ \prt_h -> do
          BL.hPut prt_h tmp_cont

      -- remove old partial file description
      removePartialFileDescription pfs

      -- figure out if upload is complete this time
      prt_h       <- liftIO $ openFile (fileLocation fd) ReadMode
      actualsize  <- liftIO $ hFileSize prt_h
      actualchksm <- md5 <$> liftIO (BL.hGetContents prt_h)

      if actualsize < fsize then do

        -- add new partial file description
        let pfs' = PartialFileStatus actualsize actualchksm
        fd' <- newPartialFile (fileLocation fd) fd pfs'

        -- PARTIAL CONTENT response
        resp partialContent . toResponse $ partialFileDescription pfs' fd'

       else if fchksm /= actualchksm then do

        -- TODO: revert file merge
        resp conflict . toResponse $ actualchksm

       else do

        (fid, fd') <- newFile (fileLocation fd) fd
        ok $ toResponse $ fileDescription fid fd'

    _ -> Page.notFound


upload :: UpMonad m => m Response
upload = dir "file" $ do

  -- uploading file + properties
  (tmp, name, conty) <- lookFile "file"
  fsize              <- lookRead "size"
  fchksm             <- look "checksum"

  now <- liftIO getCurrentTime
  let fd = FileDescription { fileName = Text.pack name
                           , fileSize = fsize
                           , fileChecksum = fchksm
                           , fileContentType = showContentType conty
                           , fileLocation = ""
                           , fileCreatedAt = now
                           }

  acceptUpload fd tmp

acceptUpload :: UpMonad m => FileDescription -> FilePath -> m Response
acceptUpload fd tmp = do

  tmp_h       <- liftIO $ openFile tmp ReadMode
  -- actual uploaded file size
  actualsize  <- liftIO $ hFileSize tmp_h
  -- actual uploaded file checksum
  actualchksm <- md5 <$> liftIO (BL.hGetContents tmp_h)

  if actualsize < fileSize fd then do

    -- partial file upload
    let pfs = PartialFileStatus actualsize actualchksm
    fd' <- newPartialFile tmp fd pfs
    -- create PARTIAL CONTENT response
    resp partialContent . toResponse $ partialFileDescription pfs fd'

   else if actualchksm /= fileChecksum fd then do

    -- checksum mismatch: create CONFLICT response
    resp conflict . toResponse $ actualchksm

   else do

    -- complete file with matching checksum
    (fid, fd') <- newFile tmp fd
    ok $ toResponse $ fileDescription fid fd'
