{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Page.Files where

import Data.List
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base32 as Base32
import qualified Data.Text as Text
import Happstack.Server
import Network.HTTP (urlEncode)
import System.Directory
import System.IO

import Acid.Files (FileDescription(..), PartialFileStatus(..))
import Files
import Types
import qualified Page.Error as Page

files :: UpMonad m => String -> m Response
files pth

  -- delete partial uploads on request
  | ".prt" `isSuffixOf` pth = do
    method [DELETE]
    let fp = "files/" ++ pth
    exists <- liftIO $ doesFileExist fp
    if exists then do
      -- get partial file status
      fsize <- liftIO $ withFile fp ReadMode hFileSize
      let pfs = PartialFileStatus fsize (takeWhile ('.' /=) pth)

      -- remove from state & from disk
      removePartialFileDescription pfs
      liftIO $ removeFile fp

      ok $ toResponse ""
     else
      Page.notFound

  -- serve or delete regular files
  | otherwise = do
    case Base32.decode $ B8.pack pth of
      Left _err -> Page.notFound
      Right fid -> do
        mfd <- lookupFileDescription fid
        case mfd of
          Nothing -> Page.notFound
          Just fd -> msum
            [ method [GET]    >> download fid fd
            , method [DELETE] >> delete   fid fd
            ]

 where
  download fid fd = do

    -- get basic file response
    res <- serveFile (asContentType (fileContentType fd))
                     (fileLocation fd)

    -- set correct filename in content disposition header
    let contdis = "attchment; filename*=UTF-8''"
               ++ urlEncode (Text.unpack $ fileName fd)
    return $
      res { rsHeaders = setHeader "Content-Disposition" contdis
                                  (rsHeaders res)
          }

  delete fid fd = do

    -- remove physical file
    liftIO $ removeFile $ fileLocation fd
    -- remove acid state entry
    removeFileDescription fid

    -- empty response
    ok $ toResponse ""
