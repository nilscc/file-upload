{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Page.Upload where

import Control.Monad
import Control.Monad.Trans
import Data.Time
import qualified Data.Text as Text
import Happstack.Server
import System.IO

import Acid.Files
import Files
import Types
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

upload :: UpMonad m => m Response
upload = msum
  [ dir "file" $ do
      (tmp, name, conty) <- lookFile "file"
      now <- liftIO getCurrentTime
      fsize <- liftIO $ withFile tmp ReadMode hFileSize
      let fd = FileDescription { fileName = Text.pack name
                               , fileSize = fsize
                               , fileContentType = showContentType conty
                               , fileLocation = ""
                               , fileCreatedAt = now
                               }
      (fid, fd') <- newFile tmp fd
      ok $ toResponse $ fileDescription fid fd'
  ]
